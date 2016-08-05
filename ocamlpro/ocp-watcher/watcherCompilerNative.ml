(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2015 OCamlPro                                                 *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open WatcherCompilerProtocol

module Optcompile = struct
  let watch_interface ppf sourcefile outputprefix =
    Clflags.binary_annotations := true;
    Optcompile.interface ppf sourcefile outputprefix;
    let path = [ Filename.dirname sourcefile ] in
    let suffs = if !Clflags.print_types then [] else [".cmi"; ".cmti"] in
    let prods = WatcherUtils.from_suff outputprefix suffs in
    let req =
      Reg_mli {
        cs_source = fst (WatcherUtils.to_absolute sourcefile);
        cs_prod = prods;
        cs_dep = WatcherUtils.env_imports path;
        cs_cmd = WatcherUtils.context true;
      }
    in
    WatcherUtils.send_request req (List.map fst prods)

  let watch_implementation ppf sourcefile outputprefix =
    Clflags.binary_annotations := true;
    Optcompile.implementation ppf sourcefile outputprefix;
    let has_mli =
      Sys.file_exists
        ((Misc.chop_extension_if_any sourcefile) ^ !Config.interface_suffix)
    in
    let suffs = [".cmx"; ".cmt"; Config.ext_obj] in
    let suffs = if has_mli then suffs else ".cmi" :: suffs in
    let suffs = if !Clflags.print_types then [] else suffs in
    let path = [ Filename.dirname sourcefile ] in
    let prods = WatcherUtils.from_suff outputprefix suffs in
    let req =
      Reg_ml {
        cs_source = fst (WatcherUtils.to_absolute sourcefile);
        cs_prod = prods;
        cs_dep = WatcherUtils.env_imports path;
        cs_cmd = WatcherUtils.context true;
      }
    in
    WatcherUtils.send_request req (List.map fst prods)


  let interface =
    WatcherUtils.if_comp_enabled3 watch_interface Optcompile.interface

  let implementation =
    WatcherUtils.if_comp_enabled3 watch_implementation Optcompile.implementation

  let c_file = Optcompile.c_file
end

module Asmlibrarian = struct
  let watch_create_archive file_list lib_name =
    Asmlibrarian.create_archive file_list lib_name;
    let clib_name = Misc.chop_extension_if_any lib_name ^ Config.ext_lib in
    let outputs = List.map WatcherUtils.to_absolute [ lib_name; clib_name ] in
    let req =
      Reg_lib {
        ls_outputs = outputs;
        ls_objs = List.map WatcherUtils.to_absolute file_list;
        ls_cmd = WatcherUtils.context false;
      }
    in
    WatcherUtils.send_request req (List.map fst outputs)

  let create_archive =
    WatcherUtils.if_comp_enabled2
      watch_create_archive Asmlibrarian.create_archive
end

module Asmpackager = struct
#if OCAML_VERSION = "4.01.0+ocp1"
  let watch_package_files ppf files targetfile =
#else
  let watch_package_files ppf initial_env files targetfile =
#endif
    Clflags.binary_annotations := true;
#if OCAML_VERSION = "4.01.0+ocp1"
    Asmpackager.package_files ppf files targetfile;
#else
    Asmpackager.package_files ppf initial_env files targetfile;
#endif
    let suffs = [".cmx"; ".cmt"; ".cmi"; Config.ext_obj] in
    let targetfile = Filename.chop_extension targetfile in
    let outputs = WatcherUtils.from_suff targetfile suffs in
    let req =
      Reg_pack {
        ls_outputs = outputs;
        ls_objs = List.map WatcherUtils.to_absolute files;
        ls_cmd = WatcherUtils.context false;
      }
    in
    WatcherUtils.send_request req (List.map fst outputs)

    let package_files =
#if OCAML_VERSION = "4.01.0+ocp1"
   WatcherUtils.if_comp_enabled3
#else
   WatcherUtils.if_comp_enabled4
#endif
      watch_package_files Asmpackager.package_files
end

module Asmlink = struct
  let watch_link_shared ppf objfiles output_name =
    Asmlink.link_shared ppf objfiles output_name;
    let outputs = [ WatcherUtils.to_absolute output_name ] in
    let req =
      Reg_lib {
        ls_outputs = outputs;
        ls_objs = List.map WatcherUtils.to_absolute objfiles;
        ls_cmd = WatcherUtils.context false;
      }
    in
    WatcherUtils.send_request req (List.map fst outputs)

  let watch_link ppf objfiles output_name =
    Asmlink.link ppf objfiles output_name;
    let stdlib =
      if !Clflags.gprofile then "stdlib.p.cmxa" else "stdlib.cmxa" in
    let stdexit =
      if !Clflags.gprofile then "std_exit.p.cmx" else "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let outputs = [ WatcherUtils.to_absolute output_name ] in
    let req =
      Reg_exe {
        ls_outputs = outputs;
        ls_objs = List.map WatcherUtils.to_absolute objfiles;
        ls_cmd = WatcherUtils.context false;
      }
    in
    WatcherUtils.send_request req (List.map fst outputs)

  let link_shared = WatcherUtils.if_comp_enabled3
    watch_link_shared Asmlink.link_shared

  let link = WatcherUtils.if_comp_enabled3
    watch_link Asmlink.link

end
