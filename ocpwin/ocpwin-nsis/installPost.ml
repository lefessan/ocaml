(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

open Cmo_format
open CmaFile
open InstallMisc

let to_delete = ref []
let cmos = Hashtbl.create 113

let read_cmas_dir dirname =
  let cmas_dir = Filename.concat dirname cmas_dir_basename in
  let possible_cmas = Sys.readdir cmas_dir in
  Array.iter (fun possible_cma ->
    if Filename.check_suffix possible_cma ".cma" then
      let cma_file = Filename.concat cmas_dir possible_cma in
      let cma_name = Filename.chop_suffix possible_cma ".cma" in
      let cma = CmaFile.load cma_file in
      List.iter (fun cmo ->
        Hashtbl.add cmos (cma_name ^ ":" ^ cmo.cmo_unit.cu_name) cmo
      ) cma.cma_cmos;
(*      to_delete := (File,cma_file) :: !to_delete *)
  ) possible_cmas;
  to_delete := (Directory, cmas_dir) :: !to_delete;
  ()


let find_cmo modname =
  try
    Hashtbl.find cmos modname
  with Not_found ->
    Printf.fprintf stderr "Could not find module %s in:\n%!" modname;
    let list = ref [] in
    Hashtbl.iter (fun key _ ->
      list := key :: !list
    ) cmos;
    let list = List.sort compare !list in
    Printf.fprintf stderr "\t";
    List.iter (fun key -> Printf.fprintf stderr "%s " key) list;
    Printf.fprintf stderr "\n%!";
    exit 2

let find_cmo (need_debug, force_link, name) =
     let cmo = find_cmo name in
     let u = cmo.cmo_unit in
     let debug = if need_debug then cmo.cmo_debug else "" in
     Printf.fprintf stderr "with_cmo %s (debug %d/%d)\n%!" name
          (String.length debug) (String.length cmo.cmo_debug);
     let cu = { u with
       cu_force_link = force_link;
       cu_debugsize = String.length debug;
     } in
     { cmo with cmo_unit = cu; cmo_debug = debug }

let make_cma dirname filename modules custom ccobjs ccopts dllibs =
  Printf.fprintf stderr "make_cma %s\n%!" filename;
  let cmos = List.map find_cmo modules in
  let cma = {
    cma_cmos = cmos;
    cma_custom = custom;
    cma_ccopts = ccopts;
    cma_ccobjs = ccobjs;
    cma_dllibs = dllibs;
  } in
    CmaFile.save_cma (Filename.concat dirname filename) cma

let make_cmo dirname filename modules =
  match modules with
      [] ->
        Printf.fprintf stderr "-cmo %s for no module\n%!" filename;
        exit 2
    | [m] ->
      let filename = Filename.concat dirname filename in
      Printf.fprintf stderr "make_cmo %s\n%!" filename;
      CmaFile.save_cmo filename (find_cmo m)
    | _ ->
      Printf.fprintf stderr "-cmo %s for multiple modules %s\n%!" filename
        (String.concat " " (List.map (fun (_,_,m) -> m) modules));
      exit 2

let make_copy dirname src dsts =
  Printf.fprintf stderr "make_copy of %s\n%!" src;
  let src = Filename.concat dirname src in
  List.iter (fun (_,_,dst) ->
    copy_file src
      (Filename.concat dirname dst)) dsts

let make_byte dirname target srcs =
  let filename = Filename.concat dirname target in
  Printf.fprintf stderr "make_byte \"%s\" from \"%s\"\n%!"
    (String.escaped filename) (String.escaped dirname);
  let digest_dir = Filename.concat dirname cmas_dir_basename in
  let oc = open_out_bin filename in
  let buff = String.create 0x1000 in
  List.iter (fun (_,_,digest) ->
    let digest_file = Filename.concat digest_dir digest in
    let ic = open_in_bin digest_file in
    let rec copy () =
      let n = input ic buff 0 0x1000 in
      if n = 0 then () else (output oc buff 0 n; copy())
    in copy();
    close_in ic;
  ) srcs;
  close_out oc

let read_cmas_batch dirname =
  let cmas_install = Filename.concat dirname cmas_install_basename in
  to_delete := (File,cmas_install) :: !to_delete;
  let ccobjs = ref [] in
  let ccopts = ref [] in
  let dllibs = ref [] in
  let custom = ref false in
  let anon_args = ref [] in
  let need_debug = ref false in
  let force_link = ref false in
  let reset () =
    ccobjs := [];
    ccopts := [];
    dllibs := [];
    custom := false
  in

  let ic = open_in cmas_install in
  let rec iter line_num ic =
    let line = input_line ic in
    let len = String.length line in
    if len > 2 && line.[0] = '-' then
      let (cmd, target) = Misc.cut_at line ' ' in
      match cmd with
        | "-cma" ->
          let args = List.rev !anon_args in
          anon_args := [];
          make_cma dirname target args !custom !ccobjs !ccopts !dllibs;
          reset ()
        | "-cmo" ->
          let args = List.rev !anon_args in
          anon_args := [];
          make_cmo dirname target args;
          reset ()
        | "-debug" -> need_debug := true
        | "-link" -> force_link := true
        | "-ccobjs" -> ccobjs := !ccobjs @ [target]
        | "-ccopts" -> ccopts := !ccopts @ [target]
        | "-dllibs" -> dllibs := !dllibs @ [target]
        | "-custom" -> custom := (target = "true")
        | "-copy" ->
          let args = List.rev !anon_args in
          anon_args := [];
          make_copy dirname target args;
          reset ()
        | "-byte" ->
          let args = List.rev !anon_args in
          anon_args := [];
          make_byte dirname target args;
          reset ()
        | _ ->
          Printf.fprintf stderr "Unknown batch command at line %d\n%!" line_num;
          exit 2
    else begin
      anon_args := (!need_debug, !force_link, line) :: !anon_args;
      need_debug := false;
      force_link := false;
    end;
    iter (line_num+1) ic
  in
  try
    iter 0 ic
  with End_of_file ->
    close_in ic

(* [post_install dirname]
   [dirname] is of the form $INSTDIR/install-ocamlwin-4.00-mingw
   containing files/{bin,bin-install,bin-ocamlpro,cmas,cmas.install,License.txt,
                     roots/ocamlwin-4.00/...}
  We have to:
  - unpack the files
*)

let unpack_files dirname =
  read_cmas_dir dirname;
  read_cmas_batch dirname;
  List.iter (fun (kind, filename) ->
    Printf.fprintf stderr "Removing %s\n%!" filename;
    match kind with
        Directory ->
          Array.iter (fun f -> Sys.remove (Filename.concat filename f)) (Sys.readdir filename);
          Unix.rmdir filename
      | File ->
        if Sys.file_exists filename then
          Sys.remove filename)
    (List.rev !to_delete)

let rec move_files_rec src_dir dst_dir =
  let files = Sys.readdir src_dir in
  Array.iter (fun file ->
    let src_file = Filename.concat src_dir file in
    let dst_file = Filename.concat dst_dir file in

(* we have special hooks: *)
    if file = "ld.conf" then begin
      let oc = open_out dst_file in
      Printf.fprintf oc "%s\r\n" dst_dir;
      Printf.fprintf oc "%s\\stublibs\r\n" dst_dir;
      close_out oc;
      Sys.remove src_file
    end
    else
    if not (Sys.file_exists dst_file) then
      Sys.rename src_file dst_file
    else
      if Sys.is_directory src_file then begin
        move_files_rec src_file dst_file;
        Unix.rmdir src_file;
      end else begin
        Sys.remove dst_file;
        Sys.rename src_file dst_file;
      end
  ) files

(*
let post_install dirname =
  let files_dir = Filename.concat dirname "files" in
  unpack_files files_dir;

  let install_prefix = Filename.dirname dirname in
  move_files_rec files_dir install_prefix;

  let bin_install_dir = Filename.concat install_prefix "bin-install" in
  let bin_dir = Filename.concat install_prefix "bin" in
  let ocpwin_config = Filename.concat bin_dir "ocpwin-config.exe" in

  copy_file
    (Filename.concat bin_install_dir "ocpwin-config.exe")
    ocpwin_config;
  assert (OnlyWin32.command [| ocpwin_config; "-remove" |] = 0);
  assert (OnlyWin32.command [| ocpwin_config; "-update" |] = 0);
  ()
*)

let post_install dirname =
  unpack_files dirname
