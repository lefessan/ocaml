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

let rec map_filter : ('a -> 'b option) -> 'a list -> 'b list =
  fun f l ->
    match l with
    | [] -> []
    | x :: q ->
      begin
        match f x with
        | None -> map_filter f q
        | Some y -> y :: (map_filter f q)
      end

let rec find_in_path file p =
  match p with
  | [] -> raise Not_found
  | dir :: rem ->
     let f = Filename.concat dir file in
     if Sys.file_exists f then f
     else find_in_path file rem

let path () =
  try Misc.split (Sys.getenv "PATH") ':'
  with
    Not_found -> []

(* Search for the ocp-watcher-client executable first in the most likely
   locations, then in the path.
   If no suitable executable is found, raise Not_found *)
let find_ocpbw_cmd () =
  let base = "ocp-watcher-client" in
  let exe_name = if Sys.unix then base else base ^ ".exe" in
  let opt_exe_name = if Sys.unix then base ^ ".opt" else base ^ ".opt.exe" in
  let p1 =
    let exe_dir = Filename.dirname Config.executable_name in
    if Filename.basename exe_dir = "boot"
    then (* Special case for the bootstrap compiler:
            search for the watcher in parent dir *)
      let cwd = Sys.getcwd () in
      if Filename.basename cwd = "_build"
      then (* Special hack to get around the ocamlbuild hack *)
        [ Filename.dirname cwd ]
      else
        [ Filename.dirname exe_dir ]
    else
      [ exe_dir; Filename.current_dir_name ]
  in
  let p2 = path () in
  let rec aux = function
    | [] -> assert false
    | [e,p] -> find_in_path e p
    | (e,p) :: tail -> try find_in_path e p with Not_found -> aux tail
  in
  aux [opt_exe_name,p1; exe_name,p1; opt_exe_name,p2; exe_name,p2]

let has_server = ref false

let server_started = ref false

(* Unix-free version *)
let start_server () =
  server_started := true;
  try
    let exe = find_ocpbw_cmd () in
    let cmd = exe ^ " -start-daemon" in
    ignore (Ocpstd.Sys.system cmd);
    has_server := true
  with
  | Not_found ->
    Printf.eprintf "Could not find ocp-watcher-client.\n%!"

(* To find whether the watcher is enabled, look at the OCPWATCHER environment
   variable; if it is not defined, use the compiler's configuration option
   (default value set at compiler configuration time, which can be overridden
   by ocaml.config, which can itself be overridden by OCAMLPARAM). *)
let comp_enabled = lazy (
  let env_var =
    try
      let str = Sys.getenv "OCPWATCHER" in
      match str with
      | "0" | "false" -> Some false
      | "1" | "true" -> Some true
      | _ -> Some true (* Any non standard value is interpreted as true *)
    with
    | Not_found -> None
  in
  let ocaml = !Config.use_ocp_watcher in
  match env_var with
  | None -> ocaml
  | Some b -> b )

let comp_enabled () =
  Lazy.force comp_enabled

(* Use temporary files for inter-process communication, since
   no other method is available without Unix *)
let send_compiler_request req =
  if comp_enabled ()
  then begin
    if not !server_started
    then start_server ();
    if !has_server then
      begin
        let ocpbw = find_ocpbw_cmd () in
        let tempfile, oc =
          Filename.open_temp_file ~mode:[Open_binary] "ocp-watcher_compreq" ""
        in
        WatcherCompilerProtocol.serialize_request oc req;
        close_out oc;
        let exe = Config.executable_name in
        let base_dir =
          let open Filename in
          if basename (dirname exe) = "bin"
          then (* Suppose that this is an installed compiler, look in parent *)
            dirname (dirname exe)
          else if basename (dirname exe) = "boot"
          then (* Suppose that we are the ocaml bootstrap compiler *)
            let cwd = Sys.getcwd () in
            if basename cwd = "_build"
            then (* For some reason, ocamlbuild copies boot into _build instead
                    of calling the boot compiler directly... *)
              dirname cwd
            else
              dirname (dirname exe)
          else (* In other cases, assume the database is in the same directory
                  as the executable (this covers uses of the newly-created
                  compilers during ocaml compilation) *)
            dirname exe
        in
        let dbdir = Filename.concat base_dir "ocp-watcher-database" in
        let dbdir_opt = " -dbdir " ^ (Filename.quote dbdir) in
        let compreq_command =
          " -compile-request " ^ (Filename.quote tempfile)
        in
        let cmd = ocpbw ^ dbdir_opt ^ compreq_command in
        let ret = Ocpstd.Sys.system cmd in
        Sys.remove tempfile;
        ret
      end
    else
      begin
        let print_argv oc arr =
          Array.iter (fun s -> output_string oc s; output_char oc ' ') arr
        in
        Printf.eprintf
          "No ocp-watcher server available -- compilation aborted\n%!";
        Printf.eprintf
          "If you do not want to use ocp-watcher,\n\
           you can set the environment variable OCPWATCHER to 0\n%!";
        Printf.eprintf "Additional information:\n\
                        Executable: %s\n\
                        Command: %a\n\
                        Working directory: %s\n%!"
          Config.executable_name
          print_argv Sys.argv
          (Sys.getcwd ());
        2
      end
  end
  else 0

let crc_list : (string * string) list ref = ref []

let register_crc file crc =
  crc_list := (file, crc) :: !crc_list

let absolute file =
  if Filename.is_relative file then
    Filename.concat (Sys.getcwd ()) file
  else file

let absolute_path file path =
  try
    let file = Misc.find_in_path path file in
    absolute file
  with Not_found ->
    Printf.eprintf "Error in ocpwatcher: could not find %S in path:\n" file;
    List.iter (fun dir ->
      Printf.eprintf "  tried %S\n" dir) path;
    Printf.eprintf "%!";
    exit 2

let to_absolute file =
  let crc = try Some (List.assoc file !crc_list) with Not_found -> None in
  (absolute_path file !Config.load_path, crc)

let list_to_absolute = List.map to_absolute

let translate_imports path imports =
  let from_path =
    map_filter
      (fun (modname, crc) ->
         try
           Some
             (Misc.find_in_path_uncap
                (path @ !Config.load_path)
                (modname ^ ".cmi"),
             crc)
         with
         | Not_found -> None)
      imports
  in
  List.map (fun (name, crc) -> (absolute name, crc)) from_path

let rec from_suff pref = function
  | [] -> []
  | suff :: tail ->
    let rel = pref ^ suff in
    let crc = try Some (List.assoc rel !crc_list) with Not_found -> None in
    (absolute rel, crc) :: (from_suff pref tail)

(* We need the index of the argument just after the file we treated *)
let compile_arg_index () =
  match Sys.argv.(!Arg.current) with
  | "-impl" | "-intf" -> !Arg.current + 2
  | _ -> !Arg.current + 1

let imports_function : (unit -> (string * Digest.t option) list) ref =
  ref (fun () -> failwith "WatcherUtils.imports_function not initialized")

let set_imports_function f =
  imports_function := f

let env_imports p =
  translate_imports p (!imports_function ())

let send_request req outputs =
  let ret = send_compiler_request req in
  if ret > 0 then
    begin
      List.iter Sys.remove outputs;
      exit ret
    end

let context compile =
  let ctx = if compile then compile_arg_index () else !Arg.current in
  (Sys.argv, Sys.getcwd (), ctx)

let if_comp_enabled1 watch_f f a =
  if comp_enabled () then
    watch_f a
  else
    f a;
  ()

let if_comp_enabled2 watch_f f a b =
  if comp_enabled () then
    watch_f a b
  else
    f a b;
  ()

let if_comp_enabled3 watch_f f a b c =
  if comp_enabled () then
    watch_f a b c
  else
    f a b c;
  ()

let if_comp_enabled4 watch_f f a b c d =
  if comp_enabled () then
    watch_f a b c d
  else
    f a b c d;
  ()
