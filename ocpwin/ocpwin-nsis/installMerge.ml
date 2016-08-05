(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

(*
  The goal of this tool is to prepare a directory containing OCaml object
  files for installation on Windows.

  -pre-install <dirname> :
     Create a sub-directory "files", containing all the files to be installed;
     Create a file "uninstall_lines.nsi" containing the list of files to be
        uninstalled;
     In "files", create a directory "cmas" and a file "cmas.install"
  -post-install <dirname> :
     Modify <dirname> to become what should have been installed in the first
     place.

  Some more options:
  -no-split-bytecode: by default, bytecode executables are split into an header and a trailer.
    This gives an opportunity to share the bytecode part among bytecode executables
    with different architectures.
*)

open Cmo_format
open CmaFile
open InstallMisc

let merge_windows dirname =
   (* copy mingw executables to msvc executables when possible *)

   let exes = [
      "ocamlyacc.exe";
      "ocamllex.opt.exe";
      "ocamlopt.opt.exe";
      "ocamldep.opt.exe";
      "ocamlc.opt.exe"
    ] in
   let mingw_bin_dir = Filename.concat dirname "bin-mingw" in
   let msvc_bin_dir = Filename.concat dirname "bin-msvc" in
   if Sys.file_exists mingw_bin_dir && Sys.file_exists msvc_bin_dir then begin
     List.iter (fun basename ->
       let mingw_exe = Filename.concat mingw_bin_dir basename in
       let msvc_exe = Filename.concat msvc_bin_dir basename in
       if Sys.file_exists mingw_exe && Sys.file_exists msvc_exe then begin
         Printf.fprintf stderr "replace %s by %s\n%!" msvc_exe mingw_exe;
         Sys.remove msvc_exe;
         copy_file mingw_exe msvc_exe
       end
     ) exes
   end;

  (* copy binary object files from mingw to cygwin *)
   let mingw_lib_dir = Filename.concat dirname "lib-mingw" in
   let cygwin_lib_dir = Filename.concat dirname "lib-cygwin" in
   if Sys.file_exists mingw_lib_dir && Sys.file_exists cygwin_lib_dir then begin
     let rec iter dirname =
       let mingw_dir = Filename.concat mingw_lib_dir dirname in
       let cygwin_dir = Filename.concat cygwin_lib_dir dirname in
       if Sys.file_exists cygwin_dir then
       let files = Sys.readdir mingw_dir in
       Array.iter (fun file ->
         let mingw_file = Filename.concat mingw_dir file in
         let cygwin_file = Filename.concat cygwin_dir file in
         if Sys.is_directory mingw_file then
           iter (Filename.concat dirname file)
         else
         if Filename.check_suffix file ".cmxa" &&
            Sys.file_exists cygwin_file &&
            Digest.file mingw_file = Digest.file cygwin_file then
            let file_a = (Filename.chop_suffix file ".cmxa") ^ ".a" in
            let mingw_file = Filename.concat mingw_dir file_a in
            let cygwin_file = Filename.concat cygwin_dir file_a in
            if Sys.file_exists mingw_file && Sys.file_exists cygwin_file then
            begin
              Printf.fprintf stderr "replace %s by %s\n%!" cygwin_file mingw_file;
              Sys.remove cygwin_file;
              copy_file mingw_file cygwin_file
            end
       ) files
     in
     iter ""
   end


