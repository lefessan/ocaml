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

(* TODO ELF:
   * We should read the persmissions on sections from the directive in the
     assembly file
   * All sections are currently aligned with 16-align. We should just check
     whether there is an alignment directive at the beginning of the file
     (this should probably be saved in [buffer] by [assemble_section].
   * In the frametable, there is a computation (.L605 - .) + 448748744
     where .L605 is the last label from the data section. This should be
     computed statically, but it is not currently the case. Why ?
*)

  open Elf
  open ElfTypes
  open ElfTypes.ABSTRACT

  type symbol_table = {
    symtab_table : (string, symbol) Hashtbl.t;
    mutable symtab_list : elf_symbol list;
    mutable symtab_num : int;
  }


  let undefined_section = {
    sec_name = ""; sec_instrs = [||];
  }

  let int64_of_int_option = function
    | None -> 0L
    | Some n -> Int64.of_int n

  let get_symbol_num symtab sy =
    match sy.sy_num with
    | Some num -> num
    | None ->
      let num = symtab.symtab_num in
      symtab.symtab_num <- num + 1;
      sy.sy_num <- Some num;
      let (st_section, st_value) = match sy.sy_pos with
          None -> (SYM_SHN_UNDEF, 0L)
        | Some pos ->
          (SYM_SHN sy.sy_sec.sec_name, Int64.of_int pos)
      in
      let st = {
        st_name = sy.sy_name;
        st_value;
        st_size = int64_of_int_option sy.sy_size;
        st_type = (match sy.sy_type with
          | Some "@function" -> STT_FUNC
          | Some _ -> assert false
          | None ->
            if sy.sy_name = "" then
              if sy.sy_pos = None then STT_NOTYPE
              else STT_SECTION
            else STT_NOTYPE);
        st_section;
        st_bind = (if sy.sy_global then STB_GLOBAL else STB_LOCAL);
      } in
      symtab.symtab_list <- st :: symtab.symtab_list;
      num

  let record_section_local_symbols symtab sec b =
       StringMap.iter (fun _ sy ->
      match sy.sy_pos with
      | None -> ()
      | Some pos ->
        let global_name =
          if sy.sy_name = "" then sec.sec_name else sy.sy_name in
(*        Printf.eprintf "Adding symtab %S -> %S\n%!"
          global_name sy.sy_name; *)
(*        Printf.eprintf "Record local symbol %S\n%!" global_name; *)
        Hashtbl.add symtab.symtab_table global_name sy;
(*        if sy.sy_global then ignore (get_symbol_num symtab sy) *)
    ) (labels b)

  let register_section_local_symbols symtab sec b =
    List.iter
      (fun (pos, k) ->
        match k with
        | RELOC_REL32 (s, Some _, _) ->
          begin
            try
              let sy = Hashtbl.find symtab.symtab_table s in
              if not sy.sy_global then begin
(*                Printf.eprintf "Export local symbol %S\n%!" s; *)
                ignore (get_symbol_num symtab sy)
              end
            with Not_found ->
(*              Printf.eprintf "Global %S\n%!" s *)
              ()
          end
        | _ -> ()
      )
      (relocations b)

  let register_section_global_symbols symtab sec b =

    StringMap.iter (fun _ sy ->
(*
      match sy.sy_pos with
      | None -> ()
      | Some pos ->
        let global_name =
          if sy.sy_name = "" then sec.sec_name else sy.sy_name in
(*        Printf.eprintf "Adding symtab %S -> %S\n%!"
          global_name sy.sy_name; *)
(*        Hashtbl.add symtab.symtab_table global_name sy; *)
*)
        if sy.sy_global then ignore (get_symbol_num symtab sy)
    ) (labels b)

  let new_section elf s_name s_desc =
    elf.e_sections <- StringMap.add s_name { s_name; s_desc } elf.e_sections

  let get_exported_symbol_num symtab reloc_table s =
    try
      let sy = Hashtbl.find symtab.symtab_table s in
      match sy.sy_num with
      | Some num -> (num, 0L)
      | None -> (* local symbol, use offset from section symbol *)
        let sec_sy = Hashtbl.find symtab.symtab_table sy.sy_sec.sec_name in
        match sy.sy_pos with
        | None -> assert false
        | Some pos ->
          match sec_sy.sy_num with
          | None ->
            (*              Printf.eprintf "Section symbol %S (%S) has no num\n%!"
                            sec_sy.sy_name sy.sy_sec.sec_name; *)
            assert false
          | Some num -> (num, Int64.of_int pos)
    with Not_found -> (* undefined: global *)
      let sy = {
        sy_name = s;
        sy_type = None;
        sy_size = None;
        sy_global = true;
        sy_sec = undefined_section;
        sy_num = None;
        sy_pos = None;
      } in
      Hashtbl.add symtab.symtab_table s sy;
      let num = get_symbol_num symtab sy in
      (num, 0L)

(* The main difference between REL and RELA is that the [addend] of
   REL sections has to be stored in the content, not in the REL section. *)

  let rel32_of_section symtab sec b =

    let rela_relocs = ref [] in
    List.iter
      (fun (pos, k) ->
        match k with

        | RELOC_DIR64 _ -> assert false

        | RELOC_DIR32 (s, None, offset) ->
          let (st_num, st_offset) = get_exported_symbol_num symtab None s in
          let r = {
            r_sym = Int64.of_int st_num;
            r_offset = Int64.of_int pos;
            r_type = R_386_32;
            r_addend = 0L;
          } in
          rela_relocs := r :: !rela_relocs;
          add_patch b pos B32 (Int64.add offset st_offset)

        | RELOC_DIR32 _ -> assert false

        | RELOC_REL32 (s, reloc_table, offset) ->
          let (st_num, st_offset) =
            get_exported_symbol_num symtab reloc_table s in
          let r = {
            r_sym = Int64.of_int st_num;
            r_offset = Int64.of_int pos;
            r_type = (match reloc_table with
              | None -> R_386_PC32
              | _ -> assert false);
            (* addend - 4 because PC is after *)
            r_addend = 0L;
          } in
          rela_relocs := r :: !rela_relocs;
          add_patch b pos B32 (Int64.sub (Int64.add offset st_offset) 4L)
      )
      (relocations b);

    let rela_relocs = Array.of_list !rela_relocs in
    Array.sort (fun r1 r2 -> compare r1.r_offset r2.r_offset) rela_relocs;

    (SHT_REL32 {
       rela_symbols = ".symtab";
       rela_target = sec.sec_name;
       rela_relocs = rela_relocs;
     })

  let rela64_of_section symtab sec b =

    let rela_relocs = ref [] in
    List.iter
      (fun (pos, k) ->
        match k with

        | RELOC_DIR32 (s, None, offset) -> assert false
        | RELOC_DIR32 _ -> assert false

        | RELOC_DIR64 (s, None, offset) ->
          let (st_num, st_offset) = get_exported_symbol_num symtab None s in
          let r = {
            r_sym = Int64.of_int st_num;
            r_offset = Int64.of_int pos;
            r_type = R_X86_64_64;
            r_addend = Int64.add offset st_offset;
          } in
          rela_relocs := r :: !rela_relocs

        | RELOC_DIR64 _ -> assert false

        | RELOC_REL32 (s, reloc_table, offset) ->
          let (st_num, st_offset) =
            get_exported_symbol_num symtab reloc_table s in
          let r = {
            r_sym = Int64.of_int st_num;
            r_offset = Int64.of_int pos;
            r_type = (match reloc_table with
              | Some PLT -> R_X86_64_PLT32
              | Some GOTPCREL -> R_X86_64_GOTPCREL
              | None -> R_X86_64_PC32);
            (* addend - 4 because PC is after *)
            r_addend = Int64.sub (Int64.add offset st_offset) 4L;
          } in
          rela_relocs := r :: !rela_relocs
      )
      (relocations b);

    let rela_relocs = Array.of_list !rela_relocs in
    Array.sort (fun r1 r2 -> compare r1.r_offset r2.r_offset) rela_relocs;

    (SHT_RELA64 {
       rela_symbols = ".symtab";
       rela_target = sec.sec_name;
       rela_relocs = rela_relocs;
     })


  let assemble arch sections =
    if !ocamlasm_check then begin
      Printf.eprintf "Warning: Intel_elf entered\n%!";
    end;
    if !debug then
      Printf.eprintf "Intel_object.ELF.assemble\n%!";
    let elf = ElfWriter.create_popular
        (if !arch64 then ElfWriter.S_X64_Linux else ElfWriter.S_386_Linux) in

    new_section elf ".bss"  (SHT_NOBITS {
        sht_flags = [ SHF_ALLOC; SHF_WRITE ];
        sht_addr = 0L;
        sht_align = 4;
        sht_content = "";
      });

    let symtab = {
      symtab_table = Hashtbl.create 111;
      symtab_list = [];
      symtab_num = 0;
    } in

    begin (* empty symbol *)
      let sy = {
        sy_name = "";
        sy_type = None;
        sy_size = None;
        sy_global = false;
        sy_sec = undefined_section;
        sy_num = None;
        sy_pos = None;
      } in
      assert (get_symbol_num symtab sy = 0);
    end;

    let assembled_sections = StringMap.map (fun sec ->
        let buffer = assemble_section sec in

        let sy = get_symbol buffer "" in
        sy.sy_pos <- Some 0;
        ignore (get_symbol_num symtab sy);

        record_section_local_symbols symtab sec buffer;

        sec, buffer) sections in


    StringMap.iter (fun _ (sec, buffer) ->
      register_section_local_symbols symtab sec buffer;
    ) assembled_sections;

    (* At this stage, all local symbols have been generated. *)

    StringMap.iter (fun _ (sec, buffer) ->
      register_section_global_symbols symtab sec buffer;
    ) assembled_sections;

    StringMap.iter (fun _ (sec, buffer) ->
      if relocations buffer <> [] then
        match arch with
           X86 ->
           new_section elf (".rel" ^ sec.sec_name)
             (rel32_of_section symtab sec buffer)
         | X64 ->
           new_section elf (".rela" ^ sec.sec_name)
             (rela64_of_section symtab sec buffer)
    ) assembled_sections;

(* We have to create relocation sections first, because the process can
  create patches on content sections (for REL sections). *)

    StringMap.iter (fun _ (sec, buffer) ->
      (* TODO: We should directly use what is in the assembly file *)
      let sht_flags = match sec.sec_name with
        | ".text" -> [ SHF_ALLOC; SHF_EXECINSTR ]
        | ".data" -> [ SHF_ALLOC; SHF_WRITE ]
        | ".rodata" -> [ SHF_ALLOC ]
        | ".rodata.cst8" -> [ SHF_ALLOC ]
        | ".note.GNU-stack" -> []
        | name ->
          Printf.eprintf "Warning: unknown ELF section %S, setting rights = %S\n%!" name "aw";
          [ SHF_ALLOC; SHF_WRITE] in

      new_section elf sec.sec_name (SHT_PROGBITS {
          sht_flags;
          sht_addr = 0L;
          sht_align = 16; (* TODO: compute *)
          sht_content = Intel_assembler.contents buffer;
        });
    ) assembled_sections;


    let symtab = Array.of_list (List.rev symtab.symtab_list) in
    new_section elf ".symtab" (SHT_SYMTAB symtab);
    let s = ElfWriter.to_string elf in
    if !ocamlasm_check then begin
      Printf.eprintf "Warning: Intel_elf assemble ok\n%!";
    end;
    s

let _ =
  let old_final_assembler = !Intel_proc.final_assembler in
  if !ocamlasm_check then
    Printf.eprintf "Warning: Intel_elf format available\n%!";
  Intel_proc.final_assembler := (function
    | S_linux_elf
    | S_linux  ->
      if !ocamlasm_check then
        Printf.eprintf "Warning: Intel_elf chosen for Config.system = %S\n%!"
          Config.system;
      assemble
    | (S_bsd_elf
      | S_win32
      | S_win64
      | S_mingw
      | S_mingw64
      | S_macosx
      | S_gnu
      | S_cygwin
      | S_solaris
      | S_beos
      | S_unknown) as system ->
      if !ocamlasm_check then begin
        Printf.eprintf "Warning: Intel_elf not used for Config.system = %S\n%!"
          Config.system;
      end;
      old_final_assembler system
  )

let init () = ()
