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

open StringCompat
open Intel_proc
open Intel_assembler

let debug = ocamlasm_object

  open Coff

let relocate_buffer b add_relative_reloc add_absolute_reloc =

  List.iter
    (fun (pos, k) ->
      match k with

      | RELOC_DIR32 (s, None, offset) ->
        add_absolute_reloc pos s None;
        add_patch b pos B32 offset
      | RELOC_DIR32 _ -> assert false

      | RELOC_DIR64 (s, None, offset) ->
        add_absolute_reloc pos s None;
        add_patch b pos B64 offset
      | RELOC_DIR64 _ -> assert false

      | RELOC_REL32 (s, reloc_table, offset) ->
        add_relative_reloc pos s reloc_table;
        add_patch b pos B32 offset
    )
    (relocations b);

  ()

  let machine_of_arch = function
      X64 -> `x64
    | X86 -> `x86

  let create_coff arch text data : coff =
    let machine = machine_of_arch arch in
    let coff = Coff.create  machine in

    let align = match arch with
        X64 -> 0x0050_0000_l (* IMAGE_SCN_ALIGN_16BYTES *)
      | X86 -> 0x0030_0000_l (* IMAGE_SCN_ALIGN_4BYTES *)
    in
    let sects = [
      (* IMAGE_SCN_MEM_WRITE + IMAGE_SCN_MEM_READ +
         IMAGE_SCN_ALIGN_4BYTES + 401 *)
      (* Why 401 ? *)
      Section.create ".data"
        (Int32.logor align 0xC000_0040l), data;
      (* IMAGE_SCN_MEM_READ + IMAGE_SCN_MEM_EXECUTE +
         IMAGE_SCN_ALIGN_4BYTES + IMAGE_SCN_LNK_INFO + 01 *)
      (* Why 01 ? *)
      Section.create ".text"
        (Int32.logor align 0x6000_0020l), text;
    ]
    in
    let syms = Hashtbl.create 16 in
    let interns = Hashtbl.create 16 in
    let create_section (sect, b) =
      Coff.add_section coff sect;
      StringMap.iter
        (fun s sy ->
            match sy.sy_pos with
            | None -> ()
            | Some pos ->
              let sym =
                if sy.sy_global then begin
                  (* Printf.fprintf stderr "global %s\n%!" s; *)
                  Symbol.export s sect (Int32.of_int pos)
                end
                else begin
                  (* Printf.fprintf stderr "intern %s\n%!" s; *)
                  Hashtbl.replace interns s false;
                  Symbol.named_intern s sect (Int32.of_int pos)
                end
              in
              Hashtbl.replace syms s sym;
              Coff.add_symbol coff sym;
        ) (labels b)
    in
    List.iter create_section sects;
    List.iter
      (fun (sect, b) ->

        let add_local_symbol reloc pos s reloc_table =
          (* Printf.fprintf stderr "add_local_symbol %s\n%!" s; *)
          if Hashtbl.mem interns s then
            Hashtbl.replace interns s true;

          let sym =
            try Hashtbl.find syms s
            with Not_found ->
              let sym = Symbol.extern s in
              Hashtbl.replace syms s sym;
              Coff.add_symbol coff sym;
              sym
          in
          reloc machine sect (Int32.of_int pos) sym
        in

        relocate_buffer b
          (add_local_symbol Reloc.rel32) (add_local_symbol Reloc.abs);

        Section.set_text sect (Intel_assembler.contents b)
      ) sects;

    Coff.filter_symbols coff (fun s -> try Hashtbl.find interns s with Not_found -> true);
    coff

  let assemble machine sections =
    if !ocamlasm_check then begin
      Printf.eprintf "Warning: Intel_coff entered\n%!";
    end;

    let text = StringMap.find ".text" sections in
    let data = StringMap.find ".data" sections in

    let data_buffer = assemble_section data in
    let text_buffer = assemble_section text in

    let code = Coff.put (create_coff machine text_buffer data_buffer) in
    if !ocamlasm_check then begin
      Printf.eprintf "Warning: Intel_coff assemble ok\n%!";
    end;
    code


let _ =
  let old_final_assembler = !Intel_proc.final_assembler in
  if !ocamlasm_check then
    Printf.eprintf "Warning: Intel_coff format available\n%!";
  Intel_proc.final_assembler := (function
    | S_win32
    | S_win64
    | S_mingw
    | S_mingw64 ->
      if !ocamlasm_check then
        Printf.eprintf "Warning: Intel_coff chosen for Config.system = %S\n%!"
          Config.system;
      assemble
    | (S_bsd_elf
      | S_linux_elf
      | S_linux
      | S_macosx
      | S_gnu
      | S_cygwin
      | S_solaris
      | S_beos
      | S_unknown) as system ->
      if !ocamlasm_check then begin
        Printf.eprintf "Warning: Intel_coff not used for Config.system = %S\n%!"
          Config.system;
      end;

      old_final_assembler system
  )

let init () = ()
