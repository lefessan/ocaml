(***********************************************************************)
(*                                                                     *)
(*                              OCaml                                  *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)
(*
  Contributors:
  * Fabrice LE FESSANT (INRIA/OCamlPro)
*)

(* needed to compile in 4.01 *)
open StringCompat

open Intel_proc

let debug = ocamlasm_assembler


type locality =
  | Loc_near (* 8 bits offset *)
  | Loc_far  (* 32 bits offset *)

(*
TODO:

If a or-pattern contains both "Reg64 ... | Reg32 ... ", it means that
we didn't discriminate between 32 bit and 64 bit modes for that
instruction. It also means that using this instruction on a 32-bit
register in 64 bit mode will not generate the 32-bit version of the
instruction, but the 64-bit version...

*)

type reloc_kind =
  (* Relocations have an optional instruction associated in case we need
     to specify the length of a jump *)
(*  | RELOC_CONST of Intel_proc.constant * Intel_proc.data_type *)

  (* 32 bits offset usually in data section *)
  | RELOC_REL32 of string * reloc_table option * int64

  | RELOC_DIR32 of string * reloc_table option * int64
  | RELOC_DIR64 of string * reloc_table option * int64


(* The buffer is parametrized by the type of the buffer. During the
   assembling process, we use a Buffer.t, but we always use string in
   the external interface. *)

type symbol = {
  sy_name : string;
  mutable sy_type : string option;
  mutable sy_size : int option;
  mutable sy_global : bool;
  mutable sy_sec : section;
  mutable sy_pos : int option;
  mutable sy_num : int option; (* position in .symtab *)
}

type buffer = {
  sec : section;
  buf : Buffer.t;
  mutable labels : symbol StringMap.t;
  mutable patches : (int * data_size * int64) list;
  mutable relocations: (int * reloc_kind) list;
}

type local_reloc =
  | RelocCall of string * int64
  | RelocShortJump of string * int64
  | RelocLongJump of string * int * int64
  | RelocConstant of constant * data_size

type result =
    Rint of int64
  | Rabs of string * reloc_table option * int64 (* absolute label + offset *)
  | Rrel of string * reloc_table option * int64 (* relative label + offset *)

let string_of_result = function
  Rint n -> Printf.sprintf "Rint %Ld" n
  | Rabs (s, t, n) -> Printf.sprintf "Rabs (%S%s, %Ld)"
                        s (Intel_gas.string_of_table t) n
  | Rrel (s, t, n) -> Printf.sprintf "Rrel (%S%s, %Ld)"
                        s (Intel_gas.string_of_table t) n


let get_symbol b s =
  try
    StringMap.find s b.labels
  with Not_found ->
    let sy = {
      sy_name = s;
      sy_type = None;
      sy_size = None;
      sy_pos = None;
      sy_global = false;
      sy_num = None;
      sy_sec = b.sec;
    } in
    b.labels <- StringMap.add s sy b.labels;
    sy



exception AsmAborted

let string_of_buffer f arch x =
  let b = Buffer.create 113 in
  f b arch x;
  Buffer.contents b

let string_of_instr arch ins =
  let b = Buffer.create 113 in
  (match system with
    | S_win32 | S_win64 -> Intel_masm.bprint_instr
    | _ -> Intel_gas.bprint_instr) b arch ins;
  Buffer.contents b

let string_of_arg arch arg =
  let b = Buffer.create 113 in
  (match system with
    | S_win32 | S_win64 -> Intel_masm.bprint_arg
    | _ -> Intel_gas.bprint_arg)
    arch b arg;
  Buffer.contents b

module LittleEndian = struct

  let buf_int8 b i =
    Buffer.add_char b.buf (char_of_int (i land 0xff))
  let buf_int8L b iL =
    buf_int8 b (Int64.to_int iL)
  let buf_int16L b iL =
    buf_int8L b iL;
    buf_int8L b (Int64.shift_right iL 8)
  let buf_int32L b iL =
    buf_int16L b iL;
    buf_int16L b (Int64.shift_right iL 16)
  let buf_int64L b iL =
    buf_int32L b iL;
    buf_int32L b (Int64.shift_right iL 32)

  let get_int8_32 s pos = Int32.of_int (Char.code s.[pos])
  let get_int16_32 s pos =
    Int32.logor
      (get_int8_32 s pos) (Int32.shift_left (get_int8_32 s (pos+1)) 8)

  let get_int32 s pos =
    Int32.logor
      (get_int16_32 s pos) (Int32.shift_left (get_int16_32 s (pos+2)) 16)

  let str_int8_32 s pos v =
    Bytes.set s pos ( char_of_int ((Int32.to_int v) land 0xff) )

  let str_int16_32 s pos v =
    str_int8_32 s pos v;
    str_int8_32 s (pos+1) (Int32.shift_right_logical v 8)

  let str_int32 s pos v =
    str_int16_32 s pos v;
    str_int16_32 s (pos+2) (Int32.shift_right_logical v 16);
    ()


  let str_int8L s pos v =
    Bytes.set s pos ( char_of_int ((Int64.to_int v) land 0xff) )

  let str_int16L s pos v =
    str_int8L s pos v;
    str_int8L s (pos+1) (Int64.shift_right_logical v 8)

  let str_int32L s pos v =
    str_int16L s pos v;
    str_int16L s (pos+2) (Int64.shift_right_logical v 16);
    ()

  let str_int64L s pos v =
    str_int32L s pos v;
    str_int32L s (pos+4) (Int64.shift_right_logical v 32);
    ()

end
open LittleEndian


  (* When a jump has to be generated, we compare the offset between the
     source instruction and the target instruction, in number of
     instructions.

     If the offset is less than [short_jump_threshold] instructions,
     we generate a short jump during the first pass. 16 is a "safe"
     value, as most instructions are shorter than 8 bytes: [REX] +
     [OPCODE] + [MODRM] + [SIB] + [IMM32] *)

let local_relocs = ref []
let local_labels = ref StringSet.empty
let jumps = ref IntMap.empty

let new_buffer sec = {
  sec;
  buf = Buffer.create 10000;
  labels = (*StringMap.add "." dot_label*) StringMap.empty;
  relocations = [];
  patches = [];
}

let label_pos b lbl =
  match (StringMap.find lbl b.labels).sy_pos with
    None -> raise Not_found
  | Some pos ->
    pos

(* Try to compute some statically computable arithmetic expressions
  in labels, or to simplify them to a form that is encodable by
  relocations. *)
let eval_const b current_pos cst =

  let rec eval = function
    | Const (_, n) -> Rint n
    | ConstLabel (".", None) -> Rabs ("", None, 0L)
    | ConstLabel (lbl, reloc_table) ->  Rabs (lbl, reloc_table, 0L)
    | ConstFloat f -> Rint (Int64.bits_of_float (float_of_string f))
    | ConstSub (c1, c2) ->
      let c1 = eval c1 and c2 = eval c2 in
      begin match c1, c2 with
        | Rint n1, Rint n2 -> Rint (Int64.sub n1 n2)
        | Rabs (s,t, n1), Rint n2 -> Rabs (s,t, Int64.sub n1 n2)
        | Rrel (s,t, n1), Rint n2 -> Rrel (s,t, Int64.sub n1 n2)

        | Rabs ("", None, n1), Rabs ("", None, n2) -> Rint (Int64.sub n1 n2)
        | Rabs ("", None, n1), Rabs (s2, None, n2) ->
          begin
            try
              let sy2 = StringMap.find s2 b.labels in
              match sy2.sy_pos with
              | Some pos2 ->
                let pos2 = Int64.of_int pos2 in
                Rint (Int64.sub
                    (Int64.add n1 (Int64.of_int current_pos))
                    (Int64.add pos2 n2) )
              | _ -> assert false
            with Not_found ->
              assert false
          end

        | Rabs (s,t, n1), Rabs ("", None, n2) ->
          begin
            try
              let sy = StringMap.find s b.labels in
              match sy.sy_pos with
              | Some pos ->
                let pos = Int64.of_int pos in
                Rint (Int64.sub (Int64.add pos n1)
                    (Int64.add n2 (Int64.of_int current_pos))
                )
              | _ -> assert false
            with Not_found ->
              Rrel (s,t, Int64.sub n1 n2)
          end
        | Rabs (s1, t1, n1), Rabs (s2, None, n2) ->
          begin
            try
              let sy2 = StringMap.find s2 b.labels in
              try
                let sy1 = StringMap.find s1 b.labels in
                assert ( sy1.sy_sec = sy2.sy_sec );
                match sy1.sy_pos, sy2.sy_pos with
                | Some pos1, Some pos2 ->
                  let pos1 = Int64.of_int pos1 in
                  let pos2 = Int64.of_int pos2 in
                  Rint (Int64.sub (Int64.add pos1 n1) (Int64.add pos2 n2) )
                | _ -> assert false
              with Not_found ->
                match sy2.sy_pos with
                | Some pos2 ->
                  let pos2 = Int64.of_int pos2 in
                  Rrel (s1, t1, Int64.sub
                      (Int64.add n1 (Int64.of_int current_pos))
                      (Int64.add pos2 n2) )
                | _ -> assert false
            with Not_found ->
              assert false
          end
        | _ -> assert false
      end
    | ConstAdd (c1, c2) ->
      let c1 = eval c1 and c2 = eval c2 in
      begin match c1, c2 with
        | Rint n1, Rint n2 -> Rint (Int64.add n1 n2)
        | Rabs (s,t, n1), Rint n2
        | Rint n2, Rabs (s,t, n1) -> Rabs (s,t, Int64.add n1 n2)
        | Rrel (s,t, n1), Rint n2
        | Rint n2, Rrel (s,t, n1) -> Rrel (s,t, Int64.add n1 n2)

        (* TODO: we could add another case, easy to solve: adding a
           Rrel to a Rabs where the symbol is local, in which case it
           can be computed. *)
        | Rrel (s,t, n1), Rabs("", None, n2) -> Rabs (s,t, Int64.add n1 n2)

        | _ -> assert false
      end
  in
  try
    let r = eval cst in
    if !debug then
      Printf.eprintf "eval_const (%s) = %s at @%d\n%!"
        (Intel_gas.string_of_constant cst)
        (string_of_result r) current_pos;
    r

  with e ->
    Printf.eprintf "Error in eval_const (%s): exception %S\n%!"
      (Intel_gas.string_of_constant cst) (Printexc.to_string e);
    raise e

let force_jump loc kind =
  jumps := IntMap.add loc kind !jumps

let is_imm32 n =
  if Sys.word_size = 4 then true else
    let n32 = Int32.of_int n in
    let n63 = Int32.to_int n32 in
    n = n63

let is_imm32L n =
  n < 0x8000_0000L && n >= -0x8000_0000L

let is_imm8 x = x < 128 && x >= -128
let is_imm8L x = x < 128L && x >= -128L

let rd_of_regf regf = match regf with
  | XMM n -> n
  | TOS -> assert false (* TODO *)
    | ST st -> assert false (* TODO *)

  let rd_of_reg64 reg64 = match reg64 with
    | RAX -> 0
    | RCX -> 1
    | RDX -> 2
    | RBX -> 3
    | RSP -> 4
    | RBP -> 5
    | RSI -> 6
    | RDI -> 7
    | R8  -> 8
    | R9  -> 9
    | R10 -> 10
    | R11 -> 11
    | R12 -> 12
    | R13 -> 13
    | R14 -> 14
    | R15 -> 15
    | RIP -> assert false

  let rd_of_reg8 reg8 = match reg8 with
      AL -> 0
    | CL -> 1
    | DL -> 2
    | BL -> 3
    | AH -> 4
    | CH -> 5
    | DH -> 6
    | BH -> 7

    | SPL -> 4
    | BPL -> 5
    | SIL -> 6
    | DIL  -> 7
    | R8B -> 8
    | R9B -> 9
    | R10B -> 10
    | R11B -> 11
    | R12B -> 12
    | R13B -> 13
    | R14B -> 14
    | R15B -> 15

  let rd_of_reg16 reg8 = match reg8 with
      AX -> 0
    | CX -> 1
    | DX -> 2
    | BX -> 3
    | SP -> 4
    | BP -> 5
    | SI -> 6
    | DI  -> 7

    | R8W -> 8
    | R9W -> 9
    | R10W -> 10
    | R11W -> 11
    | R12W -> 12
    | R13W -> 13
    | R14W -> 14
    | R15W -> 15

  let rd_of_reg32 (R32 reg64) = rd_of_reg64 reg64

  let cd_of_condition condition = match condition with
    | O -> 0
    | NO -> 1
    | B | C | NAE -> 2
    | NB | NC | AE -> 3
    | Z | E -> 4
    | NZ | NE -> 5
    | BE | NA -> 6
    | NBE | A -> 7
    | S -> 8
    | NS -> 9
    | P | PE -> 10
    | NP | PO -> 11
    | L | NGE -> 12
    | NL | GE -> 13
    | LE | NG -> 14
    | NLE | G -> 15

  (* We should precompute a position for each label depending on
     the number of instructions: heuristics = offset_in_instrs x 7
  *)

let no_rex = 0
let rex = 0b01000000

let rexr = 0b00000100 (* extension of r *)
let rexr_reg reg = if reg > 7 then rexr else 0

let rexw = rex lor 0b00001000
let rexw_reg reg = if reg > 7 then rexw else 0

let rexx = 0b00000010
let rexx_index reg = if reg > 7 then rexx else 0

let rexb = 0b00000001
let rexb_opcode reg = if reg > 7 then rexb else 0
let rexb_rm reg = if reg > 7 then rexb else 0
let rexb_base reg = if reg > 7 then rexb else 0
let reg7 reg = reg land 0x07


  let rex_of_reg8 rexcode reg8 = match reg8 with
      AL
    | CL
    | DL
    | BL
    | AH
    | CH
    | DH
    | BH

    | R8B
    | R9B
    | R10B
    | R11B
    | R12B
    | R13B
    | R14B
    | R15B

      -> rexcode
(* TODO: we should check conformance with page 3-2, vol 2A of Intel Spec ? *)
    | SPL
    | BPL
    | SIL
    | DIL  -> rex lor rexcode

  let rex_of_reg16 rexcode = function
    | AX | CX | DX | BX | SP | BP | SI | DI -> rexcode
    | R8W|R9W|R10W|R11W|R12W|R13W|R14W|R15W -> rex lor rexcode

  let mod_rm_reg m rm reg =
    (m lsl 6) + (reg7 rm) + ((reg7 reg) lsl 3)

  let sib scale index base =
    ((match scale with
          1 -> 0
        | 2 -> 1
        | 4 -> 2
        | 8 -> 3
        | _ -> assert false) lsl  6) lor ( (reg7 index) lsl 3 ) lor ( reg7 base )


  let record_reloc b pos kind =
    b.relocations <- (pos, kind) :: b.relocations

  let declare_label b s =
(*    assert (not (StringMap.mem s b.labels)); *)
    let sy = get_symbol b s in
    assert (sy.sy_pos = None);
    let pos = Buffer.length b.buf in
    sy.sy_pos <- Some pos;
    if !debug then
      Printf.eprintf "declare_label %s at pos %d\n%!" s pos;
    ()

  let buf_opcodes b opcodes =
    List.iter (fun opcode -> buf_int8 b opcode) opcodes


  let emit_rex b rexcode =
    if !arch64 && rexcode <> 0 then buf_int8 b (rexcode lor rex)


let buf_int32_imm b = function
  | None, n ->
    assert (n <= 0xFFFF_FFFFL && n >= -0x8000_0000L);
    buf_int32L b n
  | Some (symbol, table), n ->
    assert (is_imm32L n);
    begin
      match table with
      | None ->
        record_reloc b (Buffer.length b.buf)
          (RELOC_DIR32 (symbol, None, n));
        buf_int32L b 0L
      | Some PLT
      | Some GOTPCREL -> assert false
    end

let buf_int64_imm b = function
    None, n -> buf_int64L b n
  | Some (symbol, table), n ->
    begin match table with
      | None ->
        record_reloc b (Buffer.length b.buf)
          (RELOC_DIR64 (symbol, None, n));
        buf_int64L b 0L

      | Some _ -> assert false
    end

type offset_exp =
  | OImm8 of int64
  | OImm32 of (string * reloc_table option) option * int64

let offset_exp = function
  | (None, offset) ->
    if is_imm8L offset then OImm8 offset else
    if is_imm32L offset then OImm32 (None, offset) else
      assert false
  | (Some s, offset) -> OImm32 (Some s, offset)

let buf_sym b sym offset =
  match sym with
  | None -> buf_int32L b offset
  | Some (lbl, None) -> (* TODO: assert we are in 32 bits ? *)
    record_reloc b (Buffer.length b.buf) (
      RELOC_DIR32 (lbl, None, offset));
    buf_int32L b 0L
  | Some (lbl, Some _) -> assert false

let emit_mod_rm_reg b rex opcodes rm reg =
  match rm with

  | Reg32 rm ->
    let rm = rd_of_reg32 rm in
    emit_rex b (rex lor (rexr_reg reg) lor (rexb_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  | Reg64 rm ->
    let rm = rd_of_reg64 rm in
    emit_rex b (rex lor (rexr_reg reg) lor (rexb_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  | Reg8 reg8 ->
    let rm = rd_of_reg8 reg8 in
    emit_rex b ( (rex_of_reg8 rex reg8) lor (rexr_reg reg) lor (rexb_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  | Reg16 reg16 ->
    let rm = rd_of_reg16 reg16 in
    emit_rex b ( (rex_of_reg16 rex reg16) lor (rexr_reg reg) lor (rexb_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  | Regf rm ->
    let rm = rd_of_regf rm in
    emit_rex b (rex lor (rexr_reg reg) lor (rexb_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  (* 64 bits memory access *)

  | Mem (_, mem ) ->
    begin match mem with

      | M64 (Some (base, scale, None), offset)
      | M32 (Some ((R32 base), scale, None), offset) ->
        let offset = offset_exp offset in
        begin
          match base, scale, offset with
          | (RSP|R12), 1, OImm8 0L ->
            let base = rd_of_reg64 base in

            emit_rex b
              (rex lor (rexr_reg reg) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 base reg);
            buf_int8 b (sib 1 0b100 base);

          | ( RSP|R12 ), 1, OImm8 offset8 ->
            let base = rd_of_reg64 base in

            emit_rex b
              (rex lor (rexr_reg reg) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b01 0b100 reg);
            buf_int8 b (sib 1 0b100 base);
            buf_int8L b offset8

          | (RSP|R12), 1, OImm32 (sym, offset) -> (* to 0x??(%rsp) *)
            let base = rd_of_reg64 base in

            emit_rex b
              (rex lor (rexr_reg reg) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b10 0b100 reg);
            buf_int8 b (sib 1 0b100 base);
            buf_sym b sym offset

          | ( RBP|R13 ), 1, OImm8 _ -> (* to 0x??(%rbp) *)

            (* TODO check if offset8 = 0 is enough *)
            let base = rd_of_reg64 base in

            emit_rex b
              (rex lor (rexr_reg reg) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b01 base reg);
            begin match offset with
              | OImm8 offset8 -> buf_int8L b offset8
              | _ -> assert false
            end

          | _, 1, OImm8 0L -> (* to 0x00(%r??) except %rsp and %rbp *)
            let rm = rd_of_reg64 base in

            emit_rex b (rex lor (rexr_reg reg) lor (rexb_rm rm));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 rm reg);

          | _, 1, OImm8 offset8 ->
            let rm = rd_of_reg64 base in

            emit_rex b (rex lor (rexr_reg reg) lor (rexb_rm rm));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b01 rm reg);
            buf_int8L b offset8


          | RIP, 1, OImm32 (Some (symbol, table), offset) ->

            emit_rex b (rex lor (rexr_reg reg));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 0b101 reg);
            record_reloc b (Buffer.length b.buf) (RELOC_REL32 (symbol, table, offset));
            buf_int32L b 0L


          | _, 1, OImm32 (sym, offset) ->
            let rm = rd_of_reg64 base in

            emit_rex b (rex lor (rexr_reg reg) lor (rexb_rm rm));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b10 rm reg);
            buf_sym b sym offset

          | _, _, _ ->
            let index = rd_of_reg64 base in

            emit_rex b (rex lor (rexr_reg reg) lor (rexx_index index) );
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 0b100 reg);
            buf_int8 b (sib scale index 0b101);
            begin match offset with
              | OImm8 offset8 -> buf_int32L b offset8
              | OImm32 (sym, offset) ->
                buf_sym b sym offset
            end
        end

      | M32 (Some (R32 index, scale, Some (R32 base)), offset)
      | M64 (Some (index, scale, Some base), offset)  ->
        let offset = offset_exp offset in
        assert (scale = 1 || scale = 2 || scale = 4 || scale = 8);
        begin
          match index, scale, base, offset with
          | _, _, ( RBP|R13 ), OImm8 0L -> (* to 0x00(%rbp+reg) *)
            let index = rd_of_reg64 index in
            let base = rd_of_reg64 base in
            emit_rex b
              (rex lor (rexr_reg reg) lor (rexx_index index) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b01 0b100 reg);
            buf_int8 b (sib scale index base);
            buf_int8 b 0

          | _, _, _, OImm8 0L ->
            let index = rd_of_reg64 index in
            let base = rd_of_reg64 base in
            emit_rex b
              (rex lor (rexr_reg reg) lor (rexx_index index) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 0b100 reg);
            buf_int8 b (sib scale index base)

          | _ , _, _, OImm8 offset ->
            let index = rd_of_reg64 index in
            let base = rd_of_reg64 base in
            emit_rex b
              (rex lor (rexr_reg reg) lor (rexx_index index) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b01 0b100 reg);
            buf_int8 b (sib scale index base);
            buf_int8L b offset

          | _ , _, _, OImm32 (sym, offset) ->
            let index = rd_of_reg64 index in
            let base = rd_of_reg64 base in
            emit_rex b
              (rex lor (rexr_reg reg) lor (rexx_index index) lor (rexb_base base));
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b10 0b100 reg);
            buf_int8 b (sib scale index base);
            buf_sym b sym offset
        end


      | M32 (None, offset) ->
        begin match offset_exp offset with
            OImm8 _ -> assert false
          | OImm32 (sym, offset) ->
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 0b101 reg);
            buf_sym b sym offset
        end
      | M64 (None, (Some _, _))  -> assert false
      | M64 (None, (None, _))  -> assert false
    end
  | Imm (_, _) -> assert false
  | Rel (_, _) -> assert false


let emit_movlpd b dst src =
  match dst, src with
  | Regf reg, (Mem _ as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x12 ] rm (rd_of_regf reg)
  | (Mem _  as rm), Regf reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x13 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_movapd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x28 ] rm (rd_of_regf reg)
  | (Mem _  as rm), Regf reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x29 ] rm (rd_of_regf reg)
  | _ -> assert false

  let emit_movsd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _ as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
      | (Mem _ as rm), Regf reg ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_movss b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF3;
          emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
      | (Mem _  as rm), Regf reg ->
          buf_int8 b 0xF3;
          emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_andpd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0x66;
          emit_mod_rm_reg b 0 [ 0x0f; 0x54 ] rm (rd_of_regf reg)
      | _ -> assert false

  let imm8_of_rounding rounding =
    (* Precision Mask = Normal instead of Inexact *)
    (* Rounding Select = imm8.RC instead of MXCSR.RC *)
    match rounding with
      | RoundNearest -> 0b00
      | RoundDown -> 0b01
      | RoundUp -> 0x10
      | RoundTruncate -> 0x11

  let emit_roundsd b dst rounding src =
    let rounding = imm8_of_rounding rounding in
      match dst, src with
       | Regf reg, (Regf _ | Mem _ as rm) ->
            buf_int8 b 0x66;
            emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x0B ] rm (rd_of_regf reg);
            buf_int8 b rounding
       | _ -> assert false

  let emit_addsd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x58 ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_sqrtsd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x51 ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_mulsd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x59 ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_divsd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x5E ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_subsd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x5C ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_xorpd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0x66;
          emit_mod_rm_reg b 0 [ 0x0f; 0x57 ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_CVTSI2SD b dst src =
    match dst, src with
      | Regf reg, (Reg64 _ | Mem (QWORD, _) as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b rexw [ 0x0f; 0x2A ] rm (rd_of_regf reg)
      | Regf reg, (Reg32 _ | Mem (DWORD, _) as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x2A ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_CVTSD2SI b dst src =
    match dst, src with
      | Reg64 reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b rexw [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
      | Reg32 (R32 reg), (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
      | _ -> assert false

  let emit_CVTTSD2SI b dst src =
    match dst, src with
      | Reg64 reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b rexw [ 0x0f; 0x2C ] rm (rd_of_reg64 reg)
      | Reg32 reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x2C ] rm (rd_of_reg32 reg)
      | _ -> assert false

  let emit_CVTSD2SS b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF2;
          emit_mod_rm_reg b 0 [ 0x0f; 0x5A ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_CVTSS2SD b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0xF3;
          emit_mod_rm_reg b 0 [ 0x0f; 0x5A ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_comisd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0x66;
          emit_mod_rm_reg b 0 [ 0x0f; 0x2F ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_ucomisd b dst src =
    match dst, src with
      | Regf reg, (Regf _ | Mem _  as rm) ->
          buf_int8 b 0x66;
          emit_mod_rm_reg b 0 [ 0x0f; 0x2E ] rm (rd_of_regf reg)
      | _ -> assert false

  let emit_MOV b dst src =
    match dst, src with
(* movb *)
    | Reg8 r8, Imm(B8,(None,imm)) -> buf_opcodes b [0xB0 + (reg7 (rd_of_reg8 r8))]; buf_int8L b imm

      | (Mem _  as rm) , Reg8 reg ->
          emit_mod_rm_reg b (rex_of_reg8 0 reg)
            [ 0x88 ] rm (rd_of_reg8 reg); (* no REX.W *)
(* movw *)
      | (Mem _  as rm) , Reg16 reg ->
          buf_int8 b 0x66;
          emit_mod_rm_reg b rex [ 0x89 ] rm (rd_of_reg16 reg) (* no REX.W *)
      | Reg16 reg, (Mem _  as rm) ->
          buf_int8 b 0x66;
          emit_mod_rm_reg b rex [ 0x8B ] rm (rd_of_reg16 reg) (* no REX.W *)
(* movl *)
      | Reg32 reg32, (Reg32 _ | Mem _  as rm)  ->
          let reg = rd_of_reg32 reg32 in
            emit_mod_rm_reg b 0 [ 0x8B ] rm reg
      | (Mem _  as rm), Reg32 reg32 ->
          let reg = rd_of_reg32 reg32 in
            emit_mod_rm_reg b 0 [ 0x89 ] rm reg
      | (Mem (DWORD, _)  as rm), Imm ( (B8|B16|B32), iL) ->
            emit_mod_rm_reg b 0 [ 0xC7 ] rm 0;
            buf_int32_imm b iL
      | (Mem (NO, M32 _)  as rm), Imm ( (B8|B16|B32), iL) ->
          let reg = 0 in
          emit_mod_rm_reg b 0 [ 0xC7 ] rm reg;
          buf_int32_imm b iL
      | Reg32 r32, Imm( (B8|B16|B32), iL) ->
          let reg = rd_of_reg32 r32 in
            buf_int8 b (0xB8 lor (reg7 reg) );
            buf_int32_imm b iL
      | Reg32 r32, Imm( B64, iL) -> assert false

(* movq *)
      | Reg64 reg, ((Reg64 _ | Mem _ ) as rm) ->
          emit_mod_rm_reg b rexw [ 0x8B ] rm (rd_of_reg64 reg)
      | (Mem _  as rm) , Reg64 reg ->
          emit_mod_rm_reg b rexw [ 0x89 ] rm (rd_of_reg64 reg)
      | (Mem ( _, M64 _) | Reg64 _) as rm,
           Imm ( (B8|B16|B32), iL) ->
          emit_mod_rm_reg b rexw [ 0xC7 ] rm 0;
          buf_int32_imm b iL
      | Reg64 r64, Imm (B64, iL) -> (* MOVNoneQ *)
          let reg = rd_of_reg64 r64 in
            emit_rex b (rexw lor (rexb_opcode reg) );
            buf_int8 b (0xB8 lor (reg7 reg) );
          buf_int64_imm b iL
      | _ -> assert false

  type simple_encoding =
      {
        rm8_r8 : int list;
        rm64_r64 : int list;
        r8_rm8 : int list;
        r64_rm64 : int list;
        al_imm8 : int list;
        rax_imm32 : int list;
        rm8_imm8 : int list;
        rm64_imm32 : int list;
        rm64_imm8 : int list;
        reg : int;
      }

let emit_simple_encoding enc b dst src =
  match enc, dst, src with

  (* 64 bits encodings *)

  | { rm64_r64 = opcodes}, (Reg64 _ | Mem _  as rm) , Reg64 reg ->
    emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)
  | { rm64_r64 = opcodes}, (Reg32 _ | Mem _  as rm) , Reg32 reg ->
    emit_mod_rm_reg b 0 opcodes rm (rd_of_reg32 reg)

  | { r64_rm64 = opcodes}, Reg64 reg, (Mem _  as rm) ->
    emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)
  | { r64_rm64 = opcodes}, Reg32 reg, (Mem _ as rm) ->
    emit_mod_rm_reg b 0 opcodes rm (rd_of_reg32 reg)

  | { rm64_imm8 = opcodes; reg },
    (Reg64 _ | Mem ( (NO|QWORD|REAL8) , M64 _)  as rm), Imm (B8, (None, n)) ->
    emit_mod_rm_reg b rexw opcodes rm reg;
    buf_int8L b n

  | { rm8_imm8 = opcodes; reg },
    (Reg8 _ | Mem ( BYTE , M64 _)  as rm), Imm (B8, (None, n)) ->
    emit_mod_rm_reg b rexw opcodes rm reg;
    buf_int8L b n

  | { rm64_imm8 = opcodes; reg },
    (Reg32 _
    | Mem ( (DWORD|REAL4) , _)
    | Mem ( NO , M32 _)
     as rm), Imm (B8, (None, n)) ->
    emit_mod_rm_reg b 0 opcodes rm reg;
    buf_int8L b n

  | { rax_imm32 = opcodes },
    Reg64 RAX, Imm ( (B8|B16|B32), n ) ->
    emit_rex b rexw;
    buf_opcodes b opcodes;
    buf_int32_imm b n

  | { rax_imm32 = opcodes },
    Reg32 (R32 RAX),  Imm ( (B8|B16|B32), n ) ->
    buf_opcodes b opcodes;
    buf_int32_imm b n

  | { rm64_imm32 = opcodes; reg },
    (Reg32 _
    | Mem (NO, M32 _)
    | Mem ( (DWORD|REAL4), _) as rm),  Imm ( (B8|B16|B32), n ) ->
    emit_mod_rm_reg b 0 opcodes rm reg;
    buf_int32_imm b n

  | { rm64_imm32 = opcodes; reg },
    (Reg64 _ | Mem _  as rm),  Imm ( (B8|B16|B32), n) ->
    emit_mod_rm_reg b rexw opcodes rm reg;
    buf_int32_imm b n

  | _ -> assert false

let emit_simple_encoding base reg = emit_simple_encoding {
    rm8_r8 = [base];
    rm64_r64 = [base+1];
    r8_rm8 = [base+2];
    r64_rm64 = [base+3];
    al_imm8 = [base+4];
    rax_imm32 = [base+5];
    rm8_imm8 = [0x80];
    rm64_imm32 = [0x81];
    rm64_imm8 = [0x83];
    reg = reg;
  }

let emit_ADD = emit_simple_encoding 0x00 0
let emit_OR = emit_simple_encoding  0x08 1
let emit_ADC = emit_simple_encoding 0x10 2
let emit_SBB = emit_simple_encoding 0x18 3
let emit_AND = emit_simple_encoding 0x20 4
let emit_SUB = emit_simple_encoding 0x28 5
let emit_XOR = emit_simple_encoding 0x30 6
let emit_CMP = emit_simple_encoding 0x38 7

  let emit_test b dst src =
    match dst, src with
      | (Reg32 _ | Mem _ ) as rm, Reg32 reg ->
          let reg = rd_of_reg32 reg in
          emit_mod_rm_reg b 0 [ 0x85 ] rm reg
      | (Reg64 _ | Mem _ ) as rm, Reg64 reg ->
          let reg = rd_of_reg64 reg in
          emit_mod_rm_reg b rexw [ 0x85 ] rm reg

      | Reg64 RAX,  Imm ( (B8|B16|B32), n) ->
         emit_rex b rexw;
          buf_opcodes b [ 0xA9 ];
          buf_int32_imm b n
      | Reg32 (R32 RAX),  Imm ( (B8|B16|B32), n) ->
          buf_opcodes b [ 0xA9 ];
          buf_int32_imm b n
      | (Reg32 _ | Reg64 _ | Mem _ ) as rm, Imm ( (B8|B16|B32), n ) ->
          emit_mod_rm_reg b rexw [ 0xF7 ] rm 0;
         buf_int32_imm b n
      | Reg8 AL, Imm (B8, (None, iL)) ->
          buf_opcodes b [ 0xA8 ];
          buf_int8L b iL
      | Reg8 _ as rm, Imm (B8, (None, iL)) ->
          emit_mod_rm_reg b 0 [ 0xF6 ] rm 0;
          buf_int8L b iL
      | _ -> assert false

(* 3-390 -> 452 *)
let emit_imul b dst src =
  match dst, src with
  | Some (Reg32 reg), (Reg32 _ | Mem _  as rm) ->
    let reg = rd_of_reg32 reg in
    emit_mod_rm_reg b 0 [0x0F; 0xAF] rm reg
  | Some (Reg64 reg), (Reg64 _ | Mem _  as rm) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x0F; 0xAF] rm reg
  | Some (Reg64 reg | Reg32 (R32 reg) as rm), Imm (B8, (None, iL)) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x6B] rm reg;
    buf_int8L b iL
  | Some (Reg64 reg | Reg32 (R32 reg) as rm), Imm ( (B16|B32), n ) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x69] rm reg;
    buf_int32_imm b n
  | None, (Reg64 _ | Reg32 _ | Mem _  as rm) ->
    let reg = 5 in
    emit_mod_rm_reg b rexw [0xF7] rm reg
  | _ -> assert false

let emit_idiv b dst =
  let reg = 7 in
  match dst with
  | Reg64 _ | Reg32 _ | Mem _  as rm ->
    emit_mod_rm_reg b rexw [0xF7] rm reg
  | _ -> assert false

let emit_shift reg b dst src =
  match dst, src with
  | Reg64 _ | Reg32 _ as rm, Imm (B8, (None, 1L)) ->
    emit_mod_rm_reg b rexw [ 0xD1 ] rm reg

  | Reg64 _ | Reg32 _ as rm, Imm (B8, (None, n)) ->
    emit_mod_rm_reg b rexw [ 0xC1 ] rm reg;
    buf_int8L b n

  | Reg64 _ | Reg32 _ as rm, Reg8 CL ->
    emit_mod_rm_reg b rexw [ 0xD3 ] rm reg
  | _ -> assert false

let emit_SAL b dst src = emit_shift 4 b dst src
let emit_SHL = emit_SAL
let emit_SHR b dst src = emit_shift 5 b dst src
let emit_SAR b dst src = emit_shift 7 b dst src

let record_local_reloc b local_reloc =
  let pos = Buffer.length b.buf in
  let local_reloc =  (pos, local_reloc) in
  local_relocs := local_reloc :: !local_relocs

let emit_reloc_jump near_opcodes far_opcodes b loc symbol reloc_table offset =

  if StringSet.mem symbol !local_labels
   && reloc_table = None then begin (* local_reloc *)
    let near = try IntMap.find loc !jumps with _ -> Loc_far in
    match near with
    | Loc_near ->
      buf_opcodes b near_opcodes;
      record_local_reloc b (RelocShortJump (symbol, offset));
      buf_int8L b 0L
    | Loc_far ->
      buf_opcodes b far_opcodes;
      record_local_reloc b (RelocLongJump (symbol, loc, offset));
      force_jump loc Loc_far;
      buf_int32L b 0L

  end else begin (* external symbol, must reloc *)
    buf_opcodes b far_opcodes;
    force_jump loc Loc_far;
    record_reloc b (Buffer.length b.buf)
       (RELOC_REL32 (symbol, reloc_table, offset));
    buf_int32L b 0L
  end

let emit_jmp b loc dst =
  match dst with
    Rel(_, (Some (symbol, reloc_table), offset)) ->
    emit_reloc_jump [0xEB] [0xE9] b loc symbol reloc_table offset

  | Reg64 _ | Reg32 _ | Mem _ as rm ->
    let reg = 4 in
    emit_mod_rm_reg b 0 [0xFF] rm reg (* no REX *)

  | _ -> assert false

let emit_call b dst =
  match dst with
    Rel(_, (Some (symbol, reloc_table), offset))
    ->
    buf_int8 b 0xE8;
    if StringSet.mem symbol !local_labels && reloc_table = None then
      record_local_reloc b (RelocCall (symbol, 0L))
    else (* external symbol, must reloc *)
      record_reloc b (Buffer.length b.buf)
        (RELOC_REL32 (symbol, reloc_table, offset));
    buf_int32L b 0L

  | Reg64 _ | Reg32 _ | Mem _ as rm ->
    emit_mod_rm_reg b no_rex [0xFF] rm 2

  | _ -> assert false

let emit_j b loc condition dst =
  match dst with
    Rel(_, (Some (symbol, reloc_table), offset)) ->
    let opcode_offset = cd_of_condition condition in
    emit_reloc_jump
      [0x70 + opcode_offset]
      [0x0F; 0x80 + opcode_offset]
      b loc symbol reloc_table offset

  | _ -> assert false

  let emit_cmov b condition dst src =
    match dst, src with
      | (Reg64 reg | Reg32 (R32 reg)),
        (Reg64 _ | Reg32 _ | Mem _ as rm) ->
          emit_mod_rm_reg b rexw [ 0x0F; 0x40 + cd_of_condition condition ] rm (rd_of_reg64 reg)

      | _ -> assert false

  let emit_set b condition dst =
    match dst with
        Reg8 _ as rm ->
          emit_mod_rm_reg b 0 [0x0F; 0x90 + cd_of_condition condition] rm 0
      | _ -> assert false

  let emit_movsx b dst src =
    match dst, src with
        (Reg64 reg | Reg32 (R32 reg)), (Mem (BYTE, _) | Reg8 _ as rm) ->
          let reg = rd_of_reg64 reg in
            emit_mod_rm_reg b rex [0x0F;0xBE] rm reg (* no REX.W *)
    | (Reg64 reg | Reg32 (R32 reg)), (Mem (WORD, _) | Reg16 _ as rm) ->
          let reg = rd_of_reg64 reg in
            emit_mod_rm_reg b rexw [0x0F;0xBF] rm reg
      | _ -> assert false

  let emit_movsxd b dst src =
    match dst, src with
        (Reg64 reg | Reg32 (R32 reg)), (Mem _ | Reg32 _ as rm) ->
          let reg = rd_of_reg64 reg in
            emit_mod_rm_reg b rexw [0x63] rm reg
      | _ -> assert false

let emit_MOVZX b dst src =
  match dst, src with
    (Reg64 reg | Reg32 (R32 reg)), (Mem (BYTE, _) | Reg8 _ as rm) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x0F;0xB6] rm reg
  | Reg64 reg, (Mem (WORD, _) | Reg16 _  as rm) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x0F;0xB7] rm reg
  | Reg32 (R32 reg), (Mem (WORD, _) | Reg16 _ as rm) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b 0 [0x0F;0xB7] rm reg
  | _ -> assert false

let emit_FSTP b dst =
  match dst with
  | Mem ( (REAL8|QWORD),_) as rm -> emit_mod_rm_reg b 0 [0xDD] rm 3
  | Mem (REAL4,_) as rm ->   emit_mod_rm_reg b 0 [0xD9] rm 3
  | Regf (ST i) ->(*      assert (i >= 0 && i < float_stack_size); *)
    buf_opcodes b [ 0xDD; 0xD8 + i ]
  | _ -> assert false

let emit_FST b dst =
  match dst with
  | Mem ( (REAL8|QWORD),_) as rm -> emit_mod_rm_reg b 0 [0xDD] rm 2
  | Mem (REAL4,_) as rm ->   emit_mod_rm_reg b 0 [0xD9] rm 2
  | Regf (ST i) ->(*      assert (i >= 0 && i < float_stack_size); *)
    buf_opcodes b [ 0xDD; 0xD0 + i ]
  | _ -> assert false

  let emit_neg b dst =
    match dst with
        ( (Reg64 _ | Reg32 _|Mem _) as rm) ->
            emit_mod_rm_reg b rexw [ 0xF7 ] rm 3
      | _ -> assert false

let emit_LEA b dst src =
  match dst, src with
  | Reg64 reg, (Mem _ as rm) ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [ 0x8D ] rm reg
  | Reg32 reg, (Mem _ as rm) ->
    let reg = rd_of_reg32 reg in
    emit_mod_rm_reg b 0 [ 0x8D ] rm reg
(*
        | Reg16 reg, (Mem _ as rm) ->
        let reg = rd_of_reg16 reg in
        emit_mod_rm_reg b 0 [ 0x8D ] rm reg
    *)
  | _ -> assert false

let emit_stack_reg b opcode dst =
  match dst with
    Reg64 reg ->
    let reg = rd_of_reg64 reg in
    if reg > 7 then
      emit_rex b (rex lor (rexb_opcode reg));
    buf_int8 b (opcode + reg7 reg);
  | Reg32 reg ->
    let reg = rd_of_reg32 reg in
    buf_int8 b (opcode + reg7 reg);
  | _ -> assert false

let emit_push b dst =
  match dst with
  | Reg32 _ | Reg64 _          -> emit_stack_reg b 0x50 dst
  | Mem _ as rm                -> emit_mod_rm_reg b no_rex [0xFF] rm 6
  | Imm (B8, (None,n))         -> buf_int8 b 0x6A; buf_int8L b n
  | Imm (_, offset) ->
    begin match offset_exp offset with
      | OImm8 n -> buf_int8 b 0x6A; buf_int8L b n
      | OImm32 (sym, offset) ->
        buf_int8 b 0x68; buf_sym b sym offset
    end
  | _ -> assert false

  let emit_pop b dst =
  match dst with
  | Reg32 _ | Reg64 _          -> emit_stack_reg b 0x58 dst
  | Mem _ as rm                -> emit_mod_rm_reg b no_rex [0x8F] rm 0
  | _ -> assert false


let emit_leave b = buf_int8 b 0xC9

let emit_inc b = function
  | Reg64 _ | Reg32 _ | Mem _  as rm -> emit_mod_rm_reg b rexw [0xFF] rm 0
  | _ -> assert false


let emit_DEC b = function
(* FE /1 DEC r/m8 M Valid Valid *)
  | [ Reg8 _ | Mem (BYTE, _) as rm ] -> emit_mod_rm_reg b no_rex [0xFE] rm 1
(* FF /1 DEC r/m16 M Valid Valid *)
  | [ Reg16 _ | Mem (WORD, _ ) as rm ] -> emit_mod_rm_reg b no_rex [0x66; 0xFF] rm 1
(* FF /1 DEC r/m32 M Valid Valid *)
  | [Reg32 _ | Mem (DWORD, _)  as rm] -> emit_mod_rm_reg b no_rex [0xFF] rm 1
(* REX.W + FF /1 DEC r/m64 M Valid N.E. *)
  | [Reg64 _ | Mem (QWORD, _)  as rm] -> emit_mod_rm_reg b rexw [0xFF] rm 1
  | _ -> assert false

let emit_ret b = buf_int8 b 0xC3
let emit_cqto b = emit_rex b rexw; buf_int8 b 0x99

let emit_BSWAP b = function
  | Reg32 reg -> buf_opcodes b [ 0x0F; 0xC8 + reg7 (rd_of_reg32 reg) ];
  | Reg64 reg ->
    let reg = rd_of_reg64 reg in
    emit_rex b (rexw lor (rexb_opcode reg));
    buf_opcodes b [ 0x0F; 0xC8 + reg7 reg ]
  | _ -> assert false

let emit_FLDCW b = function
  | Mem _ as rm -> emit_mod_rm_reg b no_rex [0xD9] rm 5
  | _ -> assert false

let emit_FXCH b = function
  | None -> buf_opcodes b [ 0xD9; 0xC9 ]
  | Some (Regf (ST i)) -> buf_opcodes b [ 0xD9; 0xC8+i ]
  | Some _ -> assert false

let emit_FLD b = function
  | Mem ( (REAL4|DWORD), _) as rm -> emit_mod_rm_reg b 0 [0xD9] rm 0
  | Mem ( (REAL8|QWORD), _) as rm -> emit_mod_rm_reg b 0 [0xDD] rm 0
  | Regf (ST i) -> buf_opcodes b [ 0xD9; 0xC0 + i ]
  | _ -> assert false

let emit_FCOMP b = function
  | Mem ( (REAL4|DWORD), _) as rm -> emit_mod_rm_reg b no_rex [0xD8] rm 3
  | Mem ( (REAL8|QWORD), _) as rm -> emit_mod_rm_reg b no_rex [0xDC] rm 3
  | Regf (ST i) -> buf_opcodes b [ 0xD8; 0xD8 + i ]
  | _ -> assert false

let emit_FXXX reg = fun b dst src ->
  match dst with
  | None -> begin
      match src with
      | Mem( (REAL4|DWORD), _) as rm -> emit_mod_rm_reg b no_rex [0xD8] rm reg
      | Mem( (REAL8|QWORD), _) as rm -> emit_mod_rm_reg b no_rex [0xDC] rm reg
      | _ -> assert false
    end
  | _ -> assert false

let emit_FADD = emit_FXXX 0
let emit_FMUL = emit_FXXX 1
(* let emit_FCOM = emit_FXXX 2 *)
(* let emit_FCOMP = emit_FXXX 3 *)
let emit_FSUB = emit_FXXX 4
let emit_FSUBR = emit_FXXX 5
let emit_FDIV = emit_FXXX 6
let emit_FDIVR = emit_FXXX 7


let emit_FILD b = function
  | Mem( QWORD, _ ) as rm -> emit_mod_rm_reg b no_rex [0xDF] rm 5
  | Mem( DWORD, _ ) as rm -> emit_mod_rm_reg b no_rex [0xDB] rm 0
  | Mem(  WORD, _ ) as rm -> emit_mod_rm_reg b no_rex [0xDF] rm 0
  | _ -> assert false

let emit_FISTP b = function
  | Mem(  WORD, _ ) as rm -> emit_mod_rm_reg b no_rex [0xDF] rm 3
  | Mem( DWORD, _ ) as rm -> emit_mod_rm_reg b no_rex [0xDB] rm 3
  | Mem( QWORD, _ ) as rm -> emit_mod_rm_reg b no_rex [0xDF] rm 7
  | _ -> assert false

let emit_FNSTCW b = function
  | Mem(  (NO|WORD), _ ) as rm -> emit_mod_rm_reg b no_rex [0x9B;0xD9] rm 7
  | _ -> assert false

let emit_FSTSW b = function
  | Reg16 AX -> buf_opcodes b [ 0x9B; 0xDF; 0xE0 ]
  | Mem(  (NO|WORD), _ ) as rm -> emit_mod_rm_reg b no_rex [0x9B; 0xDD] rm 7
  | _ -> assert false

let emit_FNSTSW b = function
  | Reg16 AX -> buf_opcodes b [ 0xDF; 0xE0 ]
  | Mem(  (NO|WORD), _ ) as rm -> emit_mod_rm_reg b no_rex [0xDD] rm 7
  | _ -> assert false

let emit_FXXXP opcode b = function
    None -> buf_opcodes b [ 0xDE; opcode + 1 ]
  | Some (Regf (ST i), Regf (ST 0)) -> buf_opcodes b [ 0xDE; opcode + i ]
  | _ -> assert false

let emit_FADDP b arg = emit_FXXXP 0xC0 b arg
let emit_FMULP b arg = emit_FXXXP 0xC8 b arg
let emit_FSUBRP b arg = emit_FXXXP 0xE0 b arg
let emit_FSUBP b arg = emit_FXXXP 0xE8 b arg
let emit_FDIVRP b arg = emit_FXXXP 0xF0 b arg
let emit_FDIVP b arg = emit_FXXXP 0xF8 b arg

let emit_XCHG b src dst = (* TODO: test ! *)
  match dst, src with
  | ((Reg64 _ | Mem _ ) as rm), Reg64 reg
  | Reg64 reg, (Mem _ as rm) -> (* r64, r/m64 *)
    emit_mod_rm_reg b rexw [ 0x87 ] rm (rd_of_reg64 reg)
  | ((Reg32 _ | Mem _ ) as rm), Reg32 reg
  | Reg32 reg, (Mem _ as rm)  -> (* r32, r/m32 *)
    emit_mod_rm_reg b no_rex [ 0x87 ] rm (rd_of_reg32 reg)
  | ((Reg16 _ | Mem _ ) as rm), Reg16 reg
  | Reg16 reg, (Mem _ as rm)  -> (* r16, r/m16 *)
    emit_mod_rm_reg b rex [ 0x66; 0x87 ] rm (rd_of_reg16 reg)
  | ((Reg8 _ | Mem _ ) as rm), Reg8 reg
  | Reg8 reg, (Mem _ as rm)  -> (* r8, r/m8 *)
    emit_mod_rm_reg b no_rex [ 0x86 ] rm (rd_of_reg8 reg)
  | _ -> assert false

let assemble_instr b loc = function

(* | ADC (src, dst) -> emit_ADC b dst src *)
  | ADD (src, dst) -> emit_ADD b dst src
  | ADDSD ( src, dst) -> emit_addsd b dst src
  | AND (src, dst) -> emit_AND b dst src
  | ANDPD ( src, dst) -> emit_andpd b dst src

  | BSWAP arg -> emit_BSWAP b arg

  | CALL dst -> emit_call b dst

  | CVTSI2SD ( src, dst) -> emit_CVTSI2SD b dst src
  | CVTSD2SI ( src, dst) -> emit_CVTSD2SI b dst src
  | CVTTSD2SI ( src, dst) -> emit_CVTTSD2SI b dst src
  | CVTSD2SS ( src, dst) -> emit_CVTSD2SS b dst src
  | CVTSS2SD ( src, dst) -> emit_CVTSS2SD b dst src
  | COMISD (src, dst) -> emit_comisd b dst src
  | CQTO ->  emit_cqto b
  | CMP (src, dst) -> emit_CMP b dst src
  | CMOV ( condition, src, dst) -> emit_cmov b condition dst src
  | CDQ -> buf_int8 b 0x99
  (*    | CQO -> emit_rex b rexw; buf_int8 b 0x99 *)

  | DIVSD ( src, dst) -> emit_divsd b dst src
  | DEC dst -> emit_DEC b [dst]

  | FCOMPP -> buf_opcodes b [ 0xDE; 0xD9 ]
  | FLD1 -> buf_opcodes b [ 0xD9; 0xE8 ]
  | FLDLG2 -> buf_opcodes b [ 0xD9; 0xEC ]
  | FLDLN2 -> buf_opcodes b [ 0xD9; 0xED ]
  | FLDZ -> buf_opcodes b [ 0xD9; 0xEE ]
  (*      | FLDL2T -> buf_opcodes b  [ 0xD9; 0xE9 ] *)
  (*      | FLDL2E -> buf_opcodes b [ 0xD9; 0xEA ] *)
  (*      | FLDPI -> buf_opcodes b [ 0xD9; 0xEB ] *)
  | FPATAN -> buf_opcodes b [ 0xD9; 0xF3 ]
  | FCOS -> buf_opcodes b [ 0xD9; 0xFF ]
  | FYL2X -> buf_opcodes b [ 0xD9; 0xF1 ]
  | FSIN -> buf_opcodes b [ 0xD9; 0xFE ]
  | FSQRT -> buf_opcodes b [ 0xD9; 0xFA ]
  | FPTAN -> buf_opcodes b [ 0xD9; 0xF2 ]
  (*      | FST dst -> emit_FST b dst *)
  | FSTP dst -> emit_FSTP b dst
  | FXCH arg -> emit_FXCH b arg
  | FCOMP arg -> emit_FCOMP b arg
  | FNSTSW arg -> emit_FNSTSW b arg
  | FNSTCW arg -> emit_FNSTCW b arg
  | FCHS -> buf_opcodes b [ 0xD9; 0xE0 ]
  | FABS -> buf_opcodes b [ 0xD9; 0xE1 ]
  | FADD (src, dst)  -> emit_FADD b dst src
  | FSUB (src, dst) -> emit_FSUB b dst src
  | FMUL (src, dst) -> emit_FMUL b dst src
  | FDIV (src, dst) -> emit_FDIV b dst src
  | FDIVR (src, dst) -> emit_FDIVR b dst src
  | FSUBR (src, dst) -> emit_FSUBR b dst src

  | FILD arg -> emit_FILD b arg
  | FISTP arg -> emit_FISTP b arg
  | FLD arg -> emit_FLD b arg
  | FLDCW arg -> emit_FLDCW b arg
  | FADDP (src, dst) -> emit_FADDP b (Some (dst, src))
  | FSUBP (src, dst) -> emit_FSUBP b (Some (dst, src))
  | FMULP (src, dst) -> emit_FMULP b (Some (dst, src))
  | FDIVP (src, dst) -> emit_FDIVP b (Some (dst, src))
  | FSUBRP (src, dst) ->  emit_FSUBRP b (Some (dst, src))
  | FDIVRP (src, dst) -> emit_FDIVRP b (Some (dst, src))

  | HLT -> buf_int8 b 0xF4

  | INC dst -> emit_inc b dst
  | IMUL (src, dst) -> emit_imul b dst src
  | IDIV dst -> emit_idiv b dst

  | J (condition, dst) -> emit_j b loc (* ins *) condition dst
  | JMP dst -> emit_jmp b loc dst

  | LEAVE -> emit_leave b
  | LEA (src, dst) -> emit_LEA b dst src

  | MOV (src, dst) -> emit_MOV b dst src
  | MOVAPD ( src, dst) -> emit_movapd b dst src
  | MOVLPD ( src, dst) -> emit_movlpd b dst src
  | MOVSD ( src, dst) -> emit_movsd b dst src
  | MOVSS ( src, dst) -> emit_movss b dst src
  | MULSD ( src, dst) -> emit_mulsd b dst src
  | MOVSX (src, dst) -> emit_movsx b dst src
  | MOVZX (src, dst) -> emit_MOVZX b dst src
  | MOVSXD ( src, dst) -> emit_movsxd b dst src

  | NEG dst -> emit_neg b dst
  | NOP -> buf_int8 b 0x90

  | OR (src, dst) -> emit_OR b dst src

  | PUSH dst -> emit_push b dst
  | POP dst -> emit_pop b dst

  | RET -> emit_ret b
  | ROUNDSD ( rounding, src, dst) -> emit_roundsd b dst rounding src

  | SAL (src, dst) -> emit_SAL b dst src
  | SAR (src, dst) -> emit_SAR b dst src
  | SHR (src, dst) -> emit_SHR b dst src
(*  | SHL (src, dst) -> emit_SHL b dst src *)
  | SUBSD ( src, dst) -> emit_subsd b dst src
  | SQRTSD ( src, dst) -> emit_sqrtsd b dst src
(*  | SBB (src, dst) -> emit_SBB b dst src *)
  | SUB (src, dst) -> emit_SUB b dst src
  | SET (condition, dst) -> emit_set b condition dst

  | TEST (src, dst) -> emit_test b dst src

  | UCOMISD (src, dst) -> emit_ucomisd b dst src

  | XCHG (src, dst) -> emit_XCHG b dst src
  | XOR (src, dst) -> emit_XOR b dst src
  | XORPD ( src, dst) -> emit_xorpd b dst src


let assemble_line b loc ins =
  (*      Printf.fprintf stderr "%d: %s\n" (Buffer.length b.buf)
          (Amd64_emit_print.string_of_instr ins); *)
  try
    match ins with
    | Ins instr -> assemble_instr b loc instr
    | Comment _ -> ()
    | Global s -> (get_symbol b s).sy_global <- true
    | Constant (cst, datatype) ->
      let n = match cst with
        | Const (_, n) -> n
        | _ ->
          record_local_reloc b (RelocConstant (cst, datatype));
          0L
      in
      begin
        match datatype with
          B8 -> buf_int8L b n
        | B16 ->  buf_int16L b n
        | B32 -> buf_int32L b n
        | B64 -> buf_int64L b n
      end

    | NewLabel (s,_) -> declare_label b s
    | Bytes s -> Buffer.add_string b.buf s

    | External (_, _) -> ()
    | End -> ()
    | Set (_, _) -> assert false
    | Section _ -> assert false
    | Mode386 -> assert ( system = S_win32 )
    | Model _ -> assert ( system = S_win32 )
    | Cfi_startproc -> ()
    | Cfi_endproc -> ()
    | Cfi_adjust_cfa_offset _ -> ()
    | File _ -> ()
    | Loc _ -> ()
    | Private_extern _ -> assert false
    | Indirect_symbol _ -> assert false
    | Type (lbl, kind) -> (get_symbol b lbl).sy_type <- Some kind
    | Size (lbl, cst) ->
      begin match eval_const b (Buffer.length b.buf) cst with
        Rint n ->
          (get_symbol b lbl).sy_size <- Some (Int64.to_int n)
        | _ -> assert false
      end

    | Align (data,n) -> (* TODO: Buffer.length = 0 => set section align *)
      let pos = Buffer.length b.buf in
      let current = pos mod n in
      if current > 0 then
        let n = n - current in
        if data then
          for _i = 1 to n do
            buf_int8 b 0x00
          done
        else
          begin
            match n with
            | 0 -> ()
            | 1 -> buf_int8 b 0x90;
            | 2 -> buf_opcodes b [ 0x66; 0x90 ]
            | 3 -> buf_opcodes b [ 0x0f; 0x1f; 0x00 ]
            | 4 -> buf_opcodes b [ 0x0f; 0x1f; 0x40; 0x00 ]
            | 5 -> buf_opcodes b [ 0x0f; 0x1f; 0x44; 0x00; 0x00 ]
            | 6 -> buf_opcodes b [ 0x66; 0x0f; 0x1f; 0x44 ]; buf_int16L b 0L
            | 7 -> buf_opcodes b [ 0x0f; 0x1f; 0x80 ]; buf_int32L b 0L
            | _ ->
             for _i = 9 to n do buf_int8 b 0x66 done;
             buf_opcodes b [ 0x0f; 0x1f; 0x84; 0x00 ];
             buf_int32L b 0L
          end
    | Space n -> (* TODO: in text section, should be NOP *)
      for _i = 1 to n do buf_int8 b 0 done

  with e ->
    Printf.eprintf "Exception %s:\n%!"
      (Printexc.to_string e);
    Printf.eprintf "   masm: %s%!"
      (string_of_buffer Intel_masm.bprint_instr !arch64 ins);
    Printf.eprintf "   gas : %s%!"
      (string_of_buffer Intel_gas.bprint_instr !arch64 ins);
    raise AsmAborted

let str_int32 = LittleEndian.str_int32

let add_patch b pos size v =
  b.patches <- (pos, size, v) :: b.patches

let assemble_section section =

  jumps := IntMap.empty;
  local_labels := StringSet.empty;

  Array.iter (fun instr ->
    match instr with
      NewLabel (lbl, _) ->
      local_labels := StringSet.add lbl !local_labels
    | _ -> ()
  ) section.sec_instrs;

  let rec iter_assemble current_pass =
(*    Printf.eprintf "iter_assemble %d\n%!" current_pass; *)
    let b = new_buffer section in

    local_relocs := [];

    Array.iteri (assemble_line b) section.sec_instrs;

    let shorter_jumps = ref 0 in

    List.iter (fun (pos, local_reloc) ->
      match local_reloc with

      | RelocShortJump (label, offset) ->
        let source_pos = pos + 1 in
        let target_pos = label_pos b label in
        let n = target_pos - source_pos + Int64.to_int offset in
        assert (n >= -128 && n < 128);
        add_patch b pos B8 (Int64.of_int n)

      | RelocCall (label, offset) ->
        let source_pos = pos + 4 in
        let target_pos = label_pos b label in
        let n = target_pos - source_pos in
        let togo = Int64.add (Int64.of_int n) offset in
        add_patch b pos B32 togo

      | RelocLongJump (label, ins, offset) ->
        let source_pos = pos + 4 in
        let target_pos = label_pos b label in
        let n = target_pos - source_pos in
        let togo = Int64.add (Int64.of_int n) offset in
        add_patch b pos B32 togo;
        if togo >= -110L && togo < 110L then begin
            incr shorter_jumps;
            force_jump ins Loc_near
        end

(* TODO: here, we resolve all computations in each section, i.e. we can only
   allow one external symbol per expression. We could tolerate more complex
   expressions if we delay resolution later, i.e. after all sections have
   been generated and all symbol positions are known. *)

      | RelocConstant (cst, data_size) ->
    (*        Printf.eprintf "RelocConstant (%s, %s)\n%!"
          (Intel_gas.string_of_constant cst)
          (string_of_data_size data_size); *)
        let v = eval_const b pos cst in
        match v, data_size with
        | Rint n, _ ->
          add_patch b pos data_size n
        | Rabs (lbl, reloc_table, offset), B32 ->
          assert (reloc_table = None);
          record_reloc b pos (
            RELOC_DIR32 (lbl, reloc_table, offset))
        | Rabs (lbl, reloc_table, offset), B64 ->
          assert (reloc_table = None);
          record_reloc b pos (
            RELOC_DIR64 (lbl, reloc_table, offset))

(* Relative relocation in data segment. We add an offset of 4 because
    REL32 relocations are computed with a PC at the end, while here, it
    is at the beginning. *)
        | Rrel (lbl, None, offset), B32 ->
          record_reloc b pos (
            RELOC_REL32 (lbl, None, Int64.add offset 4L))
        | Rrel _, _ -> assert false
        | Rabs _, _ -> assert false
    ) !local_relocs;

    if !shorter_jumps > 10 && current_pass < 4 then
      iter_assemble (current_pass + 1)
    else b
  in
  iter_assemble 0

(* Relocations: we should compute all non-local relocations completely at the
  end. We should keep the last string/bytes couple to avoid duplication.
  All external labels should be absolute (ConstLabelAbs), while internal
  labels should be replaced by a relative computation. The goal is to make
  all computations either absolute, or relative to the current offset.
*)

let contents b =
  let data = Buffer.contents b.buf in
  let buf = Bytes.of_string data in
  List.iter (fun (pos, nbits, v) ->
(*    Printf.eprintf "Apply patch %s @%d\n%!" (string_of_data_size nbits) pos; *)
    match nbits with
    | B64 -> LittleEndian.str_int64L buf pos v
    | B32 -> LittleEndian.str_int32L buf pos v
    | B16 -> LittleEndian.str_int16L buf pos v
    | B8 ->  Bytes.set buf pos ( char_of_int ((Int64.to_int v) land 0xff) )
  ) b.patches;
  Bytes.to_string buf


let relocations b = b.relocations
let labels b = b.labels
