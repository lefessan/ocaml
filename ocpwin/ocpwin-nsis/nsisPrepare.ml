(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

(* Although this file implements splitting libraries into multiple
   .cmo files, this feature is currently not used. Instead, we do not
   embed many cma/cmxa/exe files from Camlp4 into the installer, and
   instead, we call a manually-written script to generate these files.
   The main reason is that there is no way to split executables into
   multiple files.
*)

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

  Some more options: -no-split-bytecode: by default, bytecode
  executables are split into an header and a trailer.  This gives an
  opportunity to share the bytecode part among bytecode executables
  with different architectures.
*)

(*
  TODO: we should strip debug information from all camlp4 libraries.
*)

open Cmo_format
open CmaFile
(* open InstallMisc *)



let buff = String.create 0x1000

let copy_file f1 f2 =
  Printf.fprintf stderr "copy_file %s %s\n%!" f1 f2;
  let ic = open_in_bin f1 in
  let oc = open_out_bin f2 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy();
  close_in ic;
  close_out oc

let write_to_file f1 s =
  let oc = open_out_bin f1 in
  output_string oc s;
  close_out oc

(* remove Windows end-of-line to read on Unix systems *)
let input_line ic =
  let s = input_line ic in
  let len = String.length s in
  let s = if len > 0 && s.[len-1] = '\r' then String.sub s 0 (len-1) else s in
  if Sys.os_type = "unix" then begin
    let len = String.length s in
    for i = 0 to len-1 do
      if s.[i] = '\\' then s.[i] <- '/'
    done;
    s
  end else s

let verbose = try
    ignore (Sys.getenv "OCPWIN_DEBUG"); true
   with Not_found -> false

let split_bytecode = ref false
let load_libraries = ref false
let share_files = ref false

let target_dir = "files"
let cmas_dir_basename = "cmas"
let cmas_install_basename = "install.cmas"
let nsis_install_files = "install_lines.nsi"
let nsis_uninstall_files = "uninstall_lines.nsi"
let nsis_target_dir = "$INSTDIR"

let check_memory () = (* Gc.compact () *) ()

let cma_counter = ref 0
let cmas = ref []
let digests = Hashtbl.create 113

let cu_imports_digest digest =
#if OCAML_VERSION = "4.01.0+ocp1"
   Digest.to_hex digest
#else
   match digest with
   | None -> "--------------------"
   | Some digest -> Digest.to_hex digest
#endif

let add_to_cma cmo =
  let u = cmo.cmo_unit in
  let unit_name = u.cu_name in
  Printf.fprintf stderr "add_to_cma compact %s\n%!" unit_name;
  check_memory  ();
  let need_debug = cmo.cmo_debug <> "" in
  let force_link = u.cu_force_link in
  let rec iter list =
    match list with
        [] ->
          incr cma_counter;
          let cma_name = Printf.sprintf "cmos%d" !cma_counter in
          let t = Hashtbl.create 113 in
          cmas := !cmas @ [ (cma_name, t)];

          let key = Printf.sprintf "%s:%s" cma_name unit_name in
          Hashtbl.add t unit_name (key, cmo);
          key
      | (cma_name, t) :: tail ->
        try
          let (key, cmo2) = Hashtbl.find t unit_name in
	  let u2 = cmo2.cmo_unit in
          if
            cmo.cmo_code = cmo2.cmo_code &&
	    u.cu_reloc = u2.cu_reloc &&
	    u.cu_imports = u2.cu_imports &&
	    u.cu_primitives = u2.cu_primitives
	  then begin
           if cmo2.cmo_debug = "" && cmo.cmo_debug <> "" then
	     cmo2.cmo_debug <- cmo.cmo_debug;
           key
	  end else begin
            Printf.fprintf stderr "%s already found but\n%!" unit_name;
	    Printf.fprintf stderr "\tunits equals %b\n%!" (cmo.cmo_unit = cmo2.cmo_unit);
	    Printf.fprintf stderr "\tcu_pos equals %b\n%!" (cmo.cmo_unit.cu_pos = cmo2.cmo_unit.cu_pos);
	    Printf.fprintf stderr "\tcu_codesize equals %b\n%!" (cmo.cmo_unit.cu_codesize = cmo2.cmo_unit.cu_codesize);
	    Printf.fprintf stderr "\tcu_reloc equals %b\n%!" (cmo.cmo_unit.cu_reloc = cmo2.cmo_unit.cu_reloc);
	    Printf.fprintf stderr "\tcu_imports equals %b\n%!" (cmo.cmo_unit.cu_imports = cmo2.cmo_unit.cu_imports);
	    if cmo.cmo_unit.cu_imports <> cmo2.cmo_unit.cu_imports then begin
              let rec iter list1 list2 =
                match list1, list2 with
                  [], [] -> assert false
                | (m, _) :: _ , [] ->
                   Printf.fprintf stderr "\t\tmore import: %s\n%!" m
                | [], (m, _) :: _  ->
                   Printf.fprintf stderr "\t\tfewer import: %s\n%!" m
                | (m1, d1) :: t1 , (m2, d2) :: t2 ->
                   if m1 = m2 && d1 = d2 then iter t1 t2 else
                   Printf.fprintf stderr "\t\tdiffer: %s (%s) <> %s (%s)\n%!"
                     m1 (cu_imports_digest d1)
                     m2 (cu_imports_digest d2)
              in
              iter cmo.cmo_unit.cu_imports cmo2.cmo_unit.cu_imports
	    end;
	    Printf.fprintf stderr "\tcu_primitives equals %b\n%!" (cmo.cmo_unit.cu_primitives = cmo2.cmo_unit.cu_primitives);
	    Printf.fprintf stderr "\tcu_force_link equals %b\n%!" (cmo.cmo_unit.cu_force_link = cmo2.cmo_unit.cu_force_link);
	    Printf.fprintf stderr "\tcu_debug equals %b\n%!" (cmo.cmo_unit.cu_debug = cmo2.cmo_unit.cu_debug);
	    Printf.fprintf stderr "\tcu_debugsize equals %b\n%!" (cmo.cmo_unit.cu_pos = cmo2.cmo_unit.cu_debugsize);

	    Printf.fprintf stderr "\tcode equals %b\n%!" (cmo.cmo_code = cmo2.cmo_code);
	    Printf.fprintf stderr "\tdebug equals %b\n%!" (cmo.cmo_debug = cmo2.cmo_debug);

            iter tail
          end
        with Not_found ->
          let key = Printf.sprintf "%s:%s" cma_name unit_name in
          Hashtbl.add t unit_name (key, cmo);
          key
  in
  let key = [ iter !cmas ] in
  let key = if force_link then "-link true" :: key else key in
  if need_debug then "-debug true" :: key else key

let need_dir dirname =
  if not ( Sys.file_exists dirname ) then Unix.mkdir dirname 0o755




let remember dirname file =
  let digest = Digest.file file in
  let digest = Digest.to_hex digest in
  let digest_file = Filename.concat dirname digest in
  if Sys.file_exists digest_file then
    Sys.remove file
  else
    Sys.rename file digest_file;
  digest

let pre_install ~target_dir ~nsis_dir from_dirname =


  let to_uninstall = ref [] in
  let post_install = ref [] in

  if Sys.file_exists target_dir then begin
    Printf.fprintf stderr "Error: target directory %s exists.\n%!" target_dir;
    exit 2
  end;
  let target_cmas_dir = Filename.concat target_dir cmas_dir_basename in

  let rec iter dirname target_dir local_dir installer_dir =
    need_dir target_dir;

    let files = Sys.readdir dirname in
    Array.iter (fun basename ->
      check_memory  ();
      let fullname = Filename.concat dirname basename in
      if Sys.is_directory fullname then
        let target_dir = Filename.concat target_dir basename in
        let installer_dir = Filename.concat installer_dir basename in
        let local_dir = Filename.concat local_dir basename in
        iter fullname target_dir local_dir installer_dir
      else
        let installer_file = Printf.sprintf "%s\\%s" installer_dir basename in
        to_uninstall := (Nsis.File, installer_file) :: !to_uninstall;
        let local_file = Filename.concat local_dir basename in
        let digest = Digest.file fullname in
        try
          if not !share_files then raise Not_found;
          let (from_file, list) = Hashtbl.find digests digest in
          list := local_file :: !list
        with Not_found ->
          Hashtbl.add digests digest (local_file, ref []);
          try
            if !load_libraries && Filename.check_suffix basename ".cma" then begin
              Printf.fprintf stderr "loading %s\n%!" fullname;
              let cma = CmaFile.load fullname in
              check_memory  ();
              let cmos = List.flatten (List.map add_to_cma cma.cma_cmos) in
              post_install := !post_install @
                cmos @
                (if cma.cma_custom then [ "-custom true" ] else [] ) @
                (List.map (fun s -> Printf.sprintf "-ccobjs %s" s) cma.cma_ccobjs) @
                (List.map (fun s -> Printf.sprintf "-ccopts %s" s) cma.cma_ccopts) @
                (List.map (fun s -> Printf.sprintf "-dllibs %s" s) cma.cma_dllibs) @
                [ Printf.sprintf "-cma %s" local_file ]
            end else
              if !load_libraries && Filename.check_suffix basename ".cmo" then begin
	        Printf.fprintf stderr "loading %s\n%!" fullname;
                let cma = CmaFile.load fullname in
                match cma.cma_cmos with
                    [cmo] ->
                      let key = add_to_cma cmo in
                      post_install := !post_install @ key @
                        [
                          Printf.sprintf "-cmo %s" local_file
                        ]
                  | _ -> assert false
              end else
                if !split_bytecode && ByteFile.is_bytecode_executable fullname then
                  let raw = ByteFile.RAW.load fullname in
                  Printf.fprintf stderr "is_bytecode_executable %s\n%!" fullname;
                  let basename = Digest.to_hex digest in
                  need_dir target_cmas_dir;
                  let basename_header = basename ^ ".header" in
                  let basename_trailer = basename ^ ".trailer" in
                  let target_header = Filename.concat target_cmas_dir basename_header in
                  let target_trailer = Filename.concat target_cmas_dir basename_trailer in
                  write_to_file target_header raw.ByteFile.RAW.header;
                  ByteFile.RAW.save target_trailer { raw with ByteFile.RAW.header = "" };
                  let digest_header = remember target_cmas_dir target_header in
                  let digest_trailer = remember target_cmas_dir target_trailer in
                  post_install := !post_install @ [
                    digest_header;
                    digest_trailer;
                    Printf.sprintf "-byte %s" local_file
                  ];
                else begin
                  let target_file = Filename.concat target_dir basename in
                  copy_file fullname target_file;
                end
          with e ->
            Printf.eprintf "Error processing %s : exception %s\n%!" fullname (Printexc.to_string e);
            let target_file = Filename.concat target_dir basename in
            copy_file fullname target_file;
    ) files;
    to_uninstall := (Nsis.Directory, installer_dir) :: !to_uninstall;
  in
  iter from_dirname target_dir "" nsis_target_dir;
  Printf.fprintf stderr "compact...\n%!";
  check_memory  ();
  Printf.fprintf stderr "compact done...\n%!";
  (*  let nsis_cmas_dir = Filename.concat nsis_target_dir cmas_dir_basename in *)
  need_dir target_cmas_dir;
  List.iter (fun (cma_name, t) ->
    let cma_basename = cma_name ^ ".cma" in
    (*    let nsis_cma = Filename.concat nsis_cmas_dir cma_basename in *)
    let target_cma = Filename.concat target_cmas_dir cma_basename in
    let cma = {
      cma_cmos = [];
      cma_ccopts = [];
      cma_custom = false;
      cma_ccobjs = [];
      cma_dllibs = [];
    } in
    Hashtbl.iter (fun _ (_,cmo) ->
      cma.cma_cmos <- cmo :: cma.cma_cmos
    ) t;
    CmaFile.save_cma target_cma cma;
    check_memory  ();
  ) !cmas;

  Printf.fprintf stderr "saving to install.cmas...\n%!";
  Hashtbl.iter (fun _ (from_file, list) ->
    if !list <> [] then
      post_install := !post_install @
        !list @
        [
          Printf.sprintf "-copy %s" from_file;
        ]) digests;

  let oc = open_out (Filename.concat target_cmas_dir cmas_install_basename) in
  List.iter (fun s -> Printf.fprintf oc "%s\n%!" s) !post_install;
  close_out oc;

  Nsis.save_files_to_uninstall
    (Filename.concat nsis_dir nsis_uninstall_files) !to_uninstall;

  ()













let src = ref None
let nsis = ref None

let arg_list = Arg.align [
  "-src", Arg.String (fun s -> src := Some s), "SRCDIR Directory with files to install";
  "-nsis", Arg.String (fun s -> nsis := Some s), "NSISDIR Directory where Nsis should find its files";
  "-fake", Arg.Unit (fun () -> ()), " (obsolete)";
  "-split", Arg.Unit (fun () ->
    split_bytecode := true;
    load_libraries := true;
  ), " Split bytecode files and libraries to increase sharing";
  "-share", Arg.Unit (fun () -> share_files := true),
  " Share files with same hash";
]

let arg_usage = String.concat "\n" [
  Printf.sprintf "%s [OPTIONS] : prepare for Windows installer"
    (Filename.basename Sys.argv.(0));
  "";
  "You must at least use '-src' and '-dst' to specify the source and destination";
  "directories.";
"";
"The following options are available:";
]

let arg_anon s =
  Printf.eprintf "Error: unexpected argument %S\n" s;
  Arg.usage arg_list arg_usage;
  exit 2

let _ =
  Arg.parse arg_list arg_anon arg_usage;
  match !src, !nsis with
  | Some src, Some nsis_dir ->
    let target_dir = Filename.concat nsis_dir "files" in
    pre_install ~target_dir ~nsis_dir src;
    exit 0
  | _ ->
    Arg.usage arg_list arg_usage;
    exit 2
