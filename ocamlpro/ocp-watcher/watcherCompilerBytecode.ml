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

module Compile = struct
  let watch_interface ppf sourcefile outputprefix =
    Clflags.binary_annotations := true;
    Compile.interface ppf sourcefile outputprefix;
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
    Compile.implementation ppf sourcefile outputprefix;
    let has_mli =
      Sys.file_exists
        ((Misc.chop_extension_if_any sourcefile) ^ !Config.interface_suffix)
    in
    let suffs = [".cmo"; ".cmt"] in
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
    WatcherUtils.if_comp_enabled3 watch_interface Compile.interface

  let implementation =
    WatcherUtils.if_comp_enabled3 watch_implementation Compile.implementation

  let c_file = Compile.c_file (* TODO: maybe do something ? *)
end

module Bytelibrarian = struct
  let watch_create_archive ppf file_list lib_name =
    Bytelibrarian.create_archive ppf file_list lib_name;
    let outputs = [ WatcherUtils.to_absolute lib_name ] in
    let req =
      Reg_lib {
        ls_outputs = outputs;
        ls_objs = List.map WatcherUtils.to_absolute file_list;
        ls_cmd = WatcherUtils.context false;
      }
    in
    WatcherUtils.send_request req (List.map fst outputs)

  let create_archive =
    WatcherUtils.if_comp_enabled3
      watch_create_archive Bytelibrarian.create_archive
end

module Bytepackager = struct
#if OCAML_VERSION = "4.01.0+ocp1"
  let watch_package_files ppf files targetfile =
#else
  let watch_package_files ppf initial_env files targetfile =
#endif
    Clflags.binary_annotations := true;
#if OCAML_VERSION = "4.01.0+ocp1"
    Bytepackager.package_files ppf files targetfile;
#else
    Bytepackager.package_files ppf initial_env files targetfile;
#endif
    let suffs = [".cmo"; ".cmt"; ".cmi"] in
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
      watch_package_files Bytepackager.package_files

end

module Bytelink = struct
  let watch_link ppf objfiles output_name =
    Bytelink.link ppf objfiles output_name;
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then "stdlib.cma" :: objfiles
      else "stdlib.cma" :: (objfiles @ ["std_exit.cmo"])
    in
    let outputs = [ WatcherUtils.to_absolute output_name ] in
    let req =
      Reg_exe {
        ls_outputs = outputs;
        ls_objs = List.map WatcherUtils.to_absolute objfiles;
        ls_cmd = WatcherUtils.context false;
      }
    in
    WatcherUtils.send_request req (List.map fst outputs)

  let link = WatcherUtils.if_comp_enabled3 watch_link Bytelink.link

end
