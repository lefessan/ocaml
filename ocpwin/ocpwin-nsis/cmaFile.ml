(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

open StringCompat
open Cmo_format

(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     debugging information if any
     compilation unit descriptor *)

type cmo_file = {
    cmo_code : string;
    mutable cmo_debug : string;
    cmo_unit : Cmo_format.compilation_unit;
  }

(* Format of a .cma file:
     magic number (Config.cma_magic_number)
     absolute offset of library descriptor
     object code for first library member
     ...
     object code for last library member
     library descriptor *)

type cma_file = {
    mutable cma_cmos : cmo_file list;
    mutable cma_custom : bool;
    mutable cma_ccobjs : string list;
    mutable cma_ccopts : string list;
    mutable cma_dllibs : string list;
  }

  let verbosity = ref 0

  let string_of_ident id =
    Printf.sprintf "%s_%d/%d" id.Ident.name id.Ident.stamp id.Ident.flags

  let load_compunit ic cu =
    if !verbosity > 0 then begin
      Printf.fprintf stderr "sizeof(compilation_unit) = %d\n"
        (Obj.size (Obj.repr cu));
      Printf.fprintf stderr "\tcu_name=%s\n" cu.cu_name;
      Printf.fprintf stderr "\tcu_pos=%d\n" cu.cu_pos;
      Printf.fprintf stderr "\tcu_codesize=%d\n" cu.cu_codesize;
      Printf.fprintf stderr "\tcu_reloc:\n";
      List.iter (fun (reloc_info, pos) ->
        Printf.fprintf stderr "\t\t%d: " pos;
        match reloc_info with
        | Reloc_getglobal id -> Printf.fprintf stderr "global(%s)\n" (string_of_ident id)
        | Reloc_setglobal id -> Printf.fprintf stderr "setglobal(%s)\n" (string_of_ident id)
	| Reloc_primitive prim -> Printf.fprintf stderr "prim(%s)\n" prim
        | Reloc_literal cst -> Printf.fprintf stderr "const\n"
     ) cu.cu_reloc;
     Printf.fprintf stderr "\tcu_imports:\n";
     List.iter (
#if OCAML_VERSION = "4.01.0+ocp1"
       fun (s, digest) ->
#else
       function (s, None) ->
          Printf.fprintf stderr "\t\t------ %s\n" s
       | (s, Some digest) ->
#endif
          Printf.fprintf stderr "\t\t%s %s\n" (Digest.to_hex digest) s
      ) cu.cu_imports;
      Printf.fprintf stderr "\tcu_primitives:\n";
      List.iter (fun s ->
        Printf.fprintf stderr "\t\t%s\n" s
      ) cu.cu_primitives;
      Printf.fprintf stderr "\tcu_force_link: %b\n" cu.cu_force_link;
      Printf.fprintf stderr "\tcu_debug: %d\n" cu.cu_debug;
      Printf.fprintf stderr "\tcu_debugsize: %d\n" cu.cu_debugsize;
    end;

    let code = String.create cu.cu_codesize in
    if !verbosity > 1 then Printf.fprintf stderr "seek_in for code %d\n%!" cu.cu_pos;
    seek_in ic cu.cu_pos;
    really_input ic code 0 cu.cu_codesize;
    let debug = String.create cu.cu_debugsize in
    if !verbosity > 1 then Printf.fprintf stderr "seek_in for debug %d\n%!" cu.cu_debug;
    seek_in ic cu.cu_debug;
    really_input ic debug 0 cu.cu_debugsize;
    (* empty these fields as they are not relevant anymore *)
    {
      cmo_code = code;
      cmo_unit = { cu with
         cu_pos = 0;
         cu_debug = 0;
         cu_debugsize = 0 };
      cmo_debug = debug;
    }

  let load_cma ic =
    let unit_pos = input_binary_int ic in
    if !verbosity > 1 then
      Printf.fprintf stderr "seek_in for library %d\n%!" unit_pos;
    seek_in ic unit_pos;
    let lib = (input_value ic : Cmo_format.library) in
    { cma_cmos = List.map (load_compunit ic) lib.lib_units;
      cma_custom = lib.lib_custom;
      cma_ccobjs = lib.lib_ccobjs;
      cma_ccopts = lib.lib_ccopts;
      cma_dllibs = lib.lib_dllibs;
    }

  let load_cmo ic =
    let unit_pos = input_binary_int ic in
    if !verbosity > 1 then
      Printf.fprintf stderr "seek_in for compilation_unit %d\n%!" unit_pos;
    seek_in ic unit_pos;
    let compunit = (input_value ic : Cmo_format.compilation_unit) in
    load_compunit ic compunit

  let save_cmo filename cmo =
    Printf.fprintf stderr "Generate object file %s\n%!" filename;
    let oc = open_out_bin filename in
    output_string oc Config.cmo_magic_number;
    let pos_code =
      String.length Config.cmo_magic_number +  4 in
    let pos_debug = pos_code + String.length cmo.cmo_code in
    let pos_unit = pos_debug + String.length cmo.cmo_debug in
    output_binary_int oc pos_unit;
    output_string oc cmo.cmo_code;
    output_string oc cmo.cmo_debug;
    output_value oc ({ cmo.cmo_unit
                       with cu_pos = pos_code; cu_debug = pos_debug }
                        : Cmo_format.compilation_unit);
    close_out oc

  let load filename =
    if !verbosity > 0 then
      Printf.fprintf stderr "Loading file %s\n%!" filename;
    let ic = open_in_bin filename in
    let magic_len = String.length Config.cmo_magic_number in
    let buffer = String.create magic_len in
    really_input ic buffer 0 magic_len;
    if buffer = Config.cmo_magic_number then
      let cmo = load_cmo ic in
      close_in ic;
      { cma_cmos = [ cmo ];
        cma_custom = false;
        cma_ccobjs = [];
        cma_ccopts = [];
        cma_dllibs = [];
      }
    else
    if buffer = Config.cma_magic_number then
      let cma = load_cma ic in
      close_in ic;
      cma
    else failwith "Not a .cmo or .cma magic"

  let merge_cma cma new_cma =
    cma.cma_cmos <- cma.cma_cmos @ new_cma.cma_cmos;
    if new_cma.cma_custom then cma.cma_custom <- true;
    cma.cma_ccopts <- cma.cma_ccopts @ new_cma.cma_ccopts;
    cma.cma_ccobjs <- cma.cma_ccobjs @ new_cma.cma_ccobjs;
    cma.cma_dllibs <- cma.cma_dllibs @ new_cma.cma_dllibs;
    ()

  let save_cma filename cma =
    Printf.fprintf stderr "Generate archive %s\n%!" filename;
    let rec set_pos pos list rev =
      match list with
          [] -> (pos, List.rev rev)
        | cmo :: tail ->
          let code_pos = pos in
          let debug_pos = code_pos + String.length cmo.cmo_code in
          let end_pos = debug_pos + String.length cmo.cmo_debug in
          let u = cmo.cmo_unit in
          if !verbosity > 0 then
            Printf.fprintf stderr "cu_name = %s\n%!" u.cu_name;
          let cu = { (* cmo.cmo_unit with *)
            cu_name = u.cu_name;
            cu_codesize = u.cu_codesize;
            cu_reloc = u.cu_reloc;
            cu_imports = u.cu_imports;
            cu_primitives = u.cu_primitives;
            cu_force_link = u.cu_force_link;
            cu_debugsize = String.length cmo.cmo_debug;
            cu_pos = code_pos;
            cu_debug = debug_pos;
#if ocaml_version = version ("4.01.0+ocp1") ||ocaml_version = version ("4.02.1+ocp1")
            cu_memprof = u.cu_memprof
#endif
          } in
          set_pos end_pos tail ({ cmo_unit = cu;
                                  cmo_code = cmo.cmo_code;
                                  cmo_debug = cmo.cmo_debug;
                                 } :: rev )
    in
    let (lib_pos, cmos) =
      set_pos (String.length Config.cma_magic_number + 4) cma.cma_cmos [] in
    let oc = open_out_bin filename in
    output_string oc Config.cma_magic_number;
    output_binary_int oc lib_pos;
    List.iter (fun cmo ->
      let cu = cmo.cmo_unit in
      if !verbosity > 0 then
        Printf.fprintf stderr "Saving module %s (%d,%d)\n%!"
          cu.cu_name cu.cu_codesize cu.cu_debugsize;
      output_string oc cmo.cmo_code;
      output_string oc cmo.cmo_debug) cmos;
    output_value oc {
      lib_units = List.map (fun cmo -> cmo.cmo_unit) cmos;
      lib_custom = cma.cma_custom;
      lib_ccopts = cma.cma_ccopts;
      lib_ccobjs = cma.cma_ccobjs;
      lib_dllibs = cma.cma_dllibs;
    };
    close_out oc
