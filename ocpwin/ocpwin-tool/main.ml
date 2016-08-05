(*
Currently implemented:
* -msvc-exec CMD : call a command within MSVC settings
* -msvc MSVC_FLAGS : set MSVC flags

Could be implemented:
Configuration:
* -switch VERSION : Set OCaml version
* -config CONFIG : Either "mingw" or "msvc"
* -x86 | -x64 : ARCH Either 32 or 64 bits
Actions:
* -set-path : set PATH globally, corresponding to chosen ARCH:VERSION:CONFIG
* -unset-path : remove ocpwin from global PATH
* -exec CMD : set PATH locally and exec command
*)

open StringCompat
let verbose =
  ref (try ignore (Sys.getenv "OCPWIN_VERBOSE"); true with _ -> false)

module MinUnix = Unix
module OnlyWin32 = struct

  let rec waitpid1 pid =
    let (_, status) = Unix.waitpid [] pid in
    match status with
    | MinUnix.WEXITED n -> n
    | MinUnix.WSIGNALED n -> -n
    | MinUnix.WSTOPPED n -> -1000-n

  let rec safe_waitpid pid =
    try
      waitpid1 pid
    with MinUnix.Unix_error (MinUnix.EINTR, _, _) -> safe_waitpid pid

  let command cmd argv =
    if !verbose then
      Printf.fprintf stderr "exec %s\n%!" cmd;
    let pid = try
                Unix.create_process cmd argv
                  Unix.stdin Unix.stdout Unix.stderr
      with e ->
        Printf.fprintf Pervasives.stderr "Error \"%s\" executing %s\n%!"
          (Printexc.to_string e) cmd;
        exit 2
    in
    let status = safe_waitpid pid in
  (*    Printf.fprintf stderr "waitpid returned %d\n%!" status; *)
    status

  let simulate_exec cmd argv =
    let status = command cmd argv in
    exit status

end

module String2 : sig
  val split : string -> char -> string list
end = struct
  let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []
end

module File = struct
  let iter_lines f name =
    let ic = open_in name in
    try
      while true do
        let line = input_line ic in
        f line
      done
    with
      End_of_file -> close_in ic
    | e -> close_in ic; raise e

end

module Windows = struct
  let slashify s =
           let s = Bytes.of_string s in
           for i = 0 to Bytes.length s - 1 do
             if Bytes.get s i = '\\' then s.[i] <- '/'
           done;
           Bytes.to_string s
  let unslashify s =
           let s = Bytes.of_string s in
           for i = 0 to Bytes.length s - 1 do
             if Bytes.get s i = '/' then s.[i] <- '\\'
           done;
           Bytes.to_string s

  let split_path s = String2.split s ';'
  let unsplit_path list = String.concat ";" list
end

let safe_mkdir dir =
  try MinUnix.mkdir dir 0o775 with exn ->
    Printf.eprintf "Warning: mkdir %S failed (%s)\n%!"
      dir (Printexc.to_string exn);
    ()
let safe_remove file =
  try Sys.remove file with exn ->
    Printf.eprintf "Warning: remove %S failed (%s)\n%!"
      file (Printexc.to_string exn);
    ()
let safe_readdir dir = try Sys.readdir dir with exn ->
  Printf.eprintf "Warning: readdir %S failed (%s)\n%!"
    dir (Printexc.to_string exn);
  [||]

let my_assert loc bool =
  if not bool then
    Printf.eprintf "Assertion failed at %s\n%!" loc

let (//) = Filename.concat

let homedir =
  try
    Sys.getenv "HOMEPATH"
  with Not_found -> "/"

let print_args () =
  Printf.printf "ocpwin.exe called as:\n%!";
  Array.iteri (fun i arg ->
    Printf.printf "   arg[%d] = %S\n%!" i arg
  ) Sys.argv


let arg_force_update_wrappers = ref None
let arg_change_switch = ref true

open Win32Registry
let _HKCU = HKEY_CURRENT_USER
let _HKLM = HKEY_LOCAL_MACHINE

let user_path_key = ([| "Environment" |], "PATH", REG_EXPAND_SZ)
let global_path_key = (
  [| "SYSTEM";"CurrentControlSet";"Control";"Session Manager";"Environment" |],
  "Path", REG_EXPAND_SZ )

let get_user_PATH () =
    try
      Some (snd (Win32Registry.read_key _HKCU user_path_key))
    with _ -> None
let set_user_PATH path =
  Win32Registry.write_key _HKCU user_path_key path;
  Win32Registry.broadcast_setting_change "Environment"

let get_global_PATH () =
    try
      Some (snd (Win32Registry.read_key _HKLM global_path_key))
    with _ -> None
let set_global_PATH path =
  Win32Registry.write_key _HKLM global_path_key path;
  Win32Registry.broadcast_setting_change "Environment"

let get_PATHs () =
  let user_path = get_user_PATH () in
  let global_path = get_global_PATH () in
  (global_path, user_path)

let print_PATH () =
  let (global_path, user_path) = get_PATHs () in
  Printf.printf "Current PATH:\n";
  (match user_path with
    None -> Printf.printf "No user-specific PATH\n"
  | Some path ->
    Printf.printf "User-specific PATH: %s\n" path);
  (match global_path with
    None -> Printf.printf "No global PATH\n"
  | Some path ->
    Printf.printf "Global PATH: %s\n" path);
  ()

let key_of_revpath rev_path name dwType =
  (Array.of_list (List.rev rev_path), name, dwType)

let hkey_of_kind kind = match kind with
    | "machine" | "global" | "system" -> _HKLM
    | "user" -> _HKCU
    | _ ->
      Printf.eprintf "Error: install/uninstall argument is either %S or %S\n%!"
        "machine" "user";
      exit 2

let ocpwin_revpath =  [ "OCPWin"; "OCamlPro"; "Software" ]

let key_of_switches_head = key_of_revpath ocpwin_revpath "ListHead" REG_SZ
let key_of_switches_tail switch_name =
  let switch_reg_path = switch_name :: ocpwin_revpath in
  key_of_revpath switch_reg_path "ListTail" REG_SZ

let key_of_current_ocpwin =
  key_of_revpath ocpwin_revpath "CurrentOcpwinWrapper" REG_SZ

let get_switches hroot =
  let switches = ref StringSet.empty in
  let rec iter key =
    match  snd ( Win32Registry.read_key hroot key ) with
    | exception _ -> [] (* Nothing installed ? *)
    | "nil" -> []
    | switch_name ->
      if StringSet.mem switch_name !switches then begin
        Printf.eprintf "Warning: loop in switch list !\n%!";
        []
      end else
        let next_switch_key = key_of_switches_tail switch_name in
        switches := StringSet.add switch_name !switches;
        switch_name :: iter next_switch_key
  in
  iter key_of_switches_head

let get_user_switches () = get_switches _HKCU
let get_global_switches () = get_switches _HKLM


let list_switches () =
  let user_switches = get_user_switches () in
  let global_switches = get_global_switches () in
  let print_switches kind switches =
    Printf.printf "%s: %d switches\n" kind (List.length switches);
    List.iter (fun switch_name ->
      Printf.printf "  * %s\n" switch_name
    ) (List.sort compare switches)
  in
  print_switches "User" user_switches;
  if global_switches <> [] then
    print_switches "Global" global_switches;
  Printf.printf "%!"

let key_of_switch_instdir switch_name =
  let switch_revpath = switch_name :: ocpwin_revpath in
  key_of_revpath switch_revpath  "CurrentInstallFolder" REG_SZ

let get_switch_instdir hroot switch_name =
  let key = key_of_switch_instdir switch_name in
  try Some (snd (Win32Registry.read_key hroot key)) with _ -> None


let hkey_of_kind = function
  | "user" -> HKEY_CURRENT_USER
  | _ -> HKEY_LOCAL_MACHINE

let kind_of_hkey = function
  | HKEY_CURRENT_USER -> "user"
  | _ -> "global"

(* MultiSwitch: whether multi-switch wrappers are "enabled" or "disabled" *)
let multi_switch_key = key_of_revpath ocpwin_revpath "MultiSwitch" REG_SZ
(* CurrentSwitch: name of current switch *)
let current_switch_key = key_of_revpath ocpwin_revpath "CurrentSwitch" REG_SZ
(* CurrentBinDir: bindir of current switch, included in PATH *)
let current_bindir_key = key_of_revpath ocpwin_revpath "CurrentBinDir" REG_SZ

let multi_switch_bindir_key =
  key_of_revpath ocpwin_revpath "MultiSwitchDir" REG_SZ

let multi_switch () =
  try
    snd (Win32Registry.read_key _HKCU multi_switch_key) = "enabled"
  with _ -> false

let copy_file =
  let buf_size = 32764 in
  let buf = Bytes.create buf_size in

  fun src dst -> (* copy_file *)
    let src = open_in_bin src in
    let dst = open_out_bin dst in
    let rec iter () =
      let nread = input src buf 0 buf_size in
      if nread > 0 then begin
        output dst buf 0 nread;
        iter ()
      end

    in
    iter ();
    close_in src;
    close_out dst


module MakePathAdmin(M: sig
  val hroot: hroot
  val get_PATH : unit -> string option
  val set_PATH : string -> unit
end) = struct

  let get_wrappers_bindir () =
    try
      Some (snd (Win32Registry.read_key M.hroot multi_switch_bindir_key))
    with _ -> None

  let get_current_switch () =
    try
      Some (snd (Win32Registry.read_key M.hroot current_switch_key))
    with _ -> None

  let get_current_bindir () =
    try
      Some (snd (Win32Registry.read_key M.hroot current_bindir_key))
    with _ -> None

  let get_PATH_without_ocpwin wrappers_bindir =
    let current_path = match M.get_PATH () with
      | Some path -> Windows.split_path path
      | None -> []
    in
    match wrappers_bindir, get_current_bindir () with
    | None, None -> current_path
    | (Some bindir, None) | (None, Some bindir) ->
      List.filter (fun dir -> dir <> bindir) current_path
    | Some bindir1, Some bindir2 ->
      List.filter (fun dir ->
        dir <> bindir1 && dir <> bindir2) current_path

  let add_switch_to_PATH switch_hroot switch_name switch_bindir =
    let wrappers_bindir = get_wrappers_bindir () in
    let current_PATH = get_PATH_without_ocpwin wrappers_bindir in
    let current_bindir = Windows.unslashify switch_bindir in
    let current_PATH = current_bindir :: current_PATH in
    let current_PATH = match wrappers_bindir with
      | None -> current_PATH
      | Some wrappers_bindir -> wrappers_bindir :: current_PATH in
    M.set_PATH (Windows.unsplit_path current_PATH);
    Win32Registry.write_key M.hroot current_bindir_key current_bindir;
    Win32Registry.write_key M.hroot current_switch_key switch_name;
    ()

  let clear_PATH () =
    let wrappers_bindir = get_wrappers_bindir () in
    let current_PATH = get_PATH_without_ocpwin wrappers_bindir in
    M.set_PATH (Windows.unsplit_path current_PATH);
    my_assert "clear_PATH:bindir"
      (Win32Registry.delete_key M.hroot current_bindir_key);
    my_assert "clear_PATH:switch"
      (Win32Registry.delete_key M.hroot current_switch_key);
    ()

end

module UserPath = MakePathAdmin(struct
  let hroot = _HKCU
  let get_PATH = get_user_PATH
  let set_PATH = set_user_PATH
end)

let remove_multi_switch_wrappers () =
  let wrapdir = match UserPath.get_wrappers_bindir () with
    | None ->
      Printf.eprintf "Error: wrappers dir not defined !\n%!";
      exit 2
    | Some wrapdir -> wrapdir in
  let wrappers = safe_readdir wrapdir in
  Array.iter (fun file ->
    let file = wrapdir // file in
    safe_remove file
  ) wrappers

let update_multi_switch () =
  Printf.printf "Updating switch wrappers...\n%!";
  let wrapdir = match UserPath.get_wrappers_bindir () with
    | None ->
      Printf.eprintf "Error: wrappers dir not defined !\n%!";
      exit 2
    | Some wrapdir -> wrapdir in
  let user_switches = get_user_switches () in
  let bindirs = ref [] in
  List.iter (fun switch_name ->
    match get_switch_instdir _HKCU switch_name with
    | None -> ()
    | Some instdir ->
      bindirs := (instdir // "bin") :: !bindirs
  ) user_switches;
  let bindirs = !bindirs in
  let binaries = ref StringSet.empty in
  List.iter (fun bindir ->
    let files = safe_readdir bindir in
    Array.iter (fun file ->
      let file = String.lowercase file in
      if Filename.check_suffix file ".exe"
        && not (StringSet.mem file !binaries) then
        binaries := StringSet.add file !binaries
    ) files
  ) bindirs;
  let binaries = StringSet.remove "ocp-wrapper.exe" !binaries in

  if not (Sys.file_exists wrapdir) then safe_mkdir wrapdir;

  let files = safe_readdir wrapdir in
  let wrappers = ref StringSet.empty in
  Array.iter (fun file ->
    let file = String.lowercase file in
    if Filename.check_suffix file ".exe" then
      wrappers := StringSet.add file !wrappers
  ) files;
  let wrappers = !wrappers in

  let src_file = match !arg_force_update_wrappers with
    | None ->
      let bindir = Filename.dirname Sys.argv.(0) in
      bindir // "ocp-wrapper.exe"
    | Some src_file ->
      Printf.printf "  Forcing %s...\n%!" src_file;
      src_file
  in
  StringSet.iter (fun exe ->
    if !arg_force_update_wrappers <> None ||
      not (StringSet.mem exe wrappers) then begin
        let dst_file = wrapdir // exe in
        Printf.printf "  Wrapping %s...\n%!" dst_file;
        copy_file src_file dst_file
      end
  ) binaries;

  Printf.printf "Update done.\n%!";
  ()



let set_current_ocpwin switch_dir =
  let ocpwin = switch_dir // "bin" // "ocpwin.exe" in
  Win32Registry.write_key _HKCU key_of_current_ocpwin ocpwin

let clear_current_ocpwin () =
  my_assert "clear_current_wrapper"
    (Win32Registry.delete_key _HKCU key_of_current_ocpwin)

  (*
module GlobalPath = MakePathAdmin(struct
  let hroot = _HKLM
  let key = current_switch_key
  let get_PATH = get_global_PATH
  let set_PATH = set_global_PATH
end)
  *)


let set_switch switch_name =
  Printf.printf "Setting switch %S\n%!" switch_name;

  if multi_switch () then update_multi_switch ();

  let f hroot =
    match get_switch_instdir hroot switch_name with
    | None ->
      Printf.eprintf "Error: could not find install dir for %S\n%!" switch_name;
      exit 2
    | Some switch_dir ->
      let switch_bindir = switch_dir // "bin" in
      UserPath.add_switch_to_PATH _HKCU switch_name switch_bindir;
      set_current_ocpwin switch_dir
  in
  let user_switches = get_user_switches () in
  if List.mem switch_name user_switches then begin
    f _HKCU
  end else
    let global_switches = get_global_switches () in
    if List.mem switch_name global_switches then begin
      f _HKLM
    end else begin
      Printf.eprintf "Error: unknown switch %S\n%!" switch_name;
      list_switches ();
      exit 2;
    end


(*
  If we install globally, we have to put a path containing only "ocpwin"
  in the shared path. Once the user has started using it, it should
  update itself automatically by (1) saving the most recent version in the
  user home and (2) calling that version immediatly if it's different.
*)

let install_switch kind switch_name switch_dir =
  Printf.printf "ocpwin: install %S %S %S\n%!" kind switch_name switch_dir;
  let hroot = hkey_of_kind kind in

  (* Insert the new switch in the list. Do nothing if the switch is
     already in the list !!! *)
  Printf.printf "(1) inserting in list of switches...\n%!";
  let next_switch_key = key_of_switches_tail switch_name in

  let is_first_switch =
    try
      ignore (Win32Registry.read_key hroot next_switch_key);
      Printf.printf "  Strange: switch already exists !!!\n%!";
      false
    with _ ->
      let first_switch_value =
        try snd ( Win32Registry.read_key hroot key_of_switches_head )
        with exn ->
          Printf.printf "  First OcpWin switch to exist !\n%!";
          (*          Printf.eprintf "Exception %S while getting ListHead value\n%!"
                      (Printexc.to_string exn); *)
          "nil"
      in
      let next_switch_key = key_of_switches_tail switch_name in
      Printf.eprintf "  Setting first key...\n%!";
      Win32Registry.write_key hroot next_switch_key first_switch_value;
      Printf.eprintf "  Setting second key...\n%!";
      Win32Registry.write_key hroot key_of_switches_head switch_name;
      Printf.eprintf "  Insertion done.\n%!";

      (first_switch_value = "nil")
  in

  if is_first_switch then begin
    let wrappers_basedir =
      if Filename.basename switch_dir = switch_name then
        Filename.dirname switch_dir
      else switch_dir in
    let wrappers_bindir = wrappers_basedir // "wrappers" in
    Printf.printf "  Creating dir for wrappers %s\n%!" wrappers_bindir;
    safe_mkdir wrappers_bindir;
    Printf.printf "  Setting regkey for wrappers\n%!";
    Win32Registry.write_key hroot multi_switch_bindir_key wrappers_bindir;
  end;

  Printf.printf "(2) registering installation directory...\n%!";
  let key_instdir = key_of_switch_instdir switch_name in
  Win32Registry.write_key hroot key_instdir switch_dir;
  Printf.printf "  done.\n%!";

  Printf.printf "(5) fixing ld.conf after relocation...%!\n";
  begin
    let libdir = switch_dir // "lib" in
    let filename = libdir // "ld.conf" in
    let lines = ref [] in
    File.iter_lines (fun line ->
      let len = String.length line in
      let line =
        if len > 0 && line.[0] = '+' then
          libdir // String.sub line 1 (len-1)
        else line
      in
      lines := line :: !lines
    ) filename;
    let lines = List.rev !lines in
    let oc = open_out filename in
    List.iter (fun line -> output_string oc line; output_char oc '\n') lines;
    close_out oc
  end;

  if !arg_change_switch then begin
    Printf.printf "(4) setting new switch as current switch...\n%!";
    set_switch switch_name;
  end;

  Printf.printf "ocpwin: installation done.\n%!";
  ()

let uninstall_switch kind switch_name switch_dir =
  Printf.printf "ocpwin: uninstall %S %S %S\n%!" kind switch_name switch_dir;
  let hroot = hkey_of_kind kind in

  let not_found () =
    Printf.eprintf "Error: switch %S not found !\n%!" switch_name;
    exit 2
  in

  let rec iter key =
    match  snd ( Win32Registry.read_key hroot key ) with
    | exception _ -> not_found ()
    | "nil" -> not_found ()
    | head ->

      if head = switch_name then
        let switch_tail_key = key_of_switches_tail switch_name in
        let next_switch =
          match  snd ( Win32Registry.read_key hroot switch_tail_key ) with
          | exception _ -> "nil"
          | next_switch -> next_switch
        in
        Win32Registry.write_key hroot key next_switch;
        my_assert "uninstall_switch(1)"
          (Win32Registry.delete_key hroot switch_tail_key);
        ()
      else
        let next_switch_key = key_of_switches_tail head in
        iter next_switch_key
  in

  Printf.printf "(1) Removing current switch from list...\n%!";
  iter key_of_switches_head;

  let uninstall_last_ocpwin () =
    Printf.printf "  Last switch: removing OCPWin from user PATH...\n%!";
    UserPath.clear_PATH ();
    clear_current_ocpwin ();
    remove_multi_switch_wrappers ();
    my_assert "remove multi_switch_key"
      (Win32Registry.delete_key hroot multi_switch_bindir_key)
  in
  Printf.printf "(2) Checking if switch is in PATH...\n%!";
  begin match UserPath.get_current_switch () with
  | None -> Printf.printf "  No switch in PATH\n%!"
  | Some current_switch ->
    if current_switch = switch_name then
      begin
        match  snd ( Win32Registry.read_key hroot key_of_switches_head ) with
      | exception _ -> uninstall_last_ocpwin ()
      | "nil" -> uninstall_last_ocpwin ()
      | switch_name ->
        Printf.printf "  Switching to switch %S...\n%!" switch_name;
        set_switch switch_name
    end;
  end;

  Printf.printf "(3) unregistering installation directory...\n%!";
  let key_instdir = key_of_switch_instdir switch_name in
  my_assert "uninstall_switch(3)"
    (Win32Registry.delete_key hroot key_instdir);
  Printf.printf "  done.\n%!";

  Printf.printf "ocpwin: uninstallation done.\n%!";
  ()

let set_multi_switch enabled =
  Win32Registry.write_key _HKCU multi_switch_key
    (if enabled then "enabled" else "disabled");
  if enabled then begin
    Printf.printf "Multi-switch enabled.\n%!";
    update_multi_switch ()
  end else begin
    Printf.printf "Multi-switch disabled\n%!";
    remove_multi_switch_wrappers ();
    Printf.printf "All wrappers removed.\n%!";
    Printf.printf
      "You might need to restart a terminal to enjoy the new config.\n%!";

  end;
  (*
  let switch_name = UserPath.get_current_switch () in
  match switch_name with
  | None -> Printf.printf "Warning: no current switch !\n%!"
  | Some switch_name ->
    set_switch switch_name;
  *)
  ()

let call_command_through_wrapper command args =
  match
    try
      let switch_name = Sys.getenv "OCAML_VERSION" in
      Some switch_name
    with _ ->
      UserPath.get_current_switch ()
  with
  | None ->
    Printf.eprintf "Error: no current switch. Aborting.\n%!";
    exit 2
  | Some switch_name ->
    let instdir = get_switch_instdir _HKCU switch_name in
    match instdir with
    | None ->
      Printf.eprintf "Error: no instdir for switch %S. Aborting.\n%!"
        switch_name;
      exit 2
    | Some instdir ->

      (* We must change the PATH for wrapped executable, so that they
         execute in the environment they are expecting. *)
      let bindir = instdir // "bin" in
      begin
        let path =
          try
            Windows.split_path (Sys.getenv "PATH")
          with _ -> []
        in
        let path = Windows.unsplit_path path in
        if !verbose then Printf.eprintf "Setting PATH=%s\n%!" path;
        MinUnix.putenv "PATH" path
      end;
      (* We should also set the CAML_LD_LIBRARY_PATH *)
      let libdir = instdir // "lib" in
      begin
        let stubsdir = instdir // "lib" // "stublibs" in
        let path =
          try
            Windows.split_path (Sys.getenv "CAML_LD_LIBRARY_PATH")
          with _ -> []
        in
        let path = if not (List.mem libdir path) then
            path @ [ libdir ] else path in
        let path = if not (List.mem stubsdir path) then
            path @ [ stubsdir ] else path in
        let path = Windows.unsplit_path path in
        if !verbose then Printf.eprintf "Setting CAML_LD_LIBRARY_PATH=%s\n%!" path;
        MinUnix.putenv "CAML_LD_LIBRARY_PATH" path
      end;

      begin try
              ignore (Sys.getenv "OCAMLLIB")
        with _ ->
          MinUnix.putenv "OCAMLLIB" libdir
      end;

      let cmd = match command with
        | Some ocamlrun ->
          bindir // ocamlrun
        | None ->
          let command = Filename.basename args.(0) in
          let command = bindir // command in

          (* Here, we replace the wrapper present as args.(0) by the
             real command being called. This is necessary for bytecode
             programs, as they will call ocamlrun and ocamlrun should
             not try to find the bytecode instructions in the
             wrapper... *)
          args.(0) <- command;
          command
      in
      if !verbose then
        Printf.eprintf "Calling command %S\n%!" cmd;
      OnlyWin32.simulate_exec cmd args






























type action =
| MsvcExec
| ListSwitches
| InstallSwitch
| UninstallSwitch
| SetSwitch
| PrintPath
| MultiSwitch
| Wrapper

let action = ref None

let string_of_action = function
  | MsvcExec -> "-msvc-exec"
  | ListSwitches -> "-list"
  | InstallSwitch -> "-installer"
  | UninstallSwitch -> "-uninstaller"
  | SetSwitch -> "-switch"
  | PrintPath -> "-print-PATH"
  | MultiSwitch -> "-multi-switch"
  | Wrapper -> "-wrapper"

let set_action a () =
  match !action with
  | None -> action := Some a
  | Some b ->
    Printf.eprintf "Error: conflicting action arguments %S and %S\n%!"
      (string_of_action b) (string_of_action a);
    exit 2

let detect_msvc = ref "auto"
let vc_exec_args = ref []
let add_vc_exec_arg s = vc_exec_args := s :: !vc_exec_args
let ocpwin_install_kind = ref ""
let ocpwin_install_version = ref ""
let ocpwin_install_dir = ref ""
let current_switch = ref ""
let multi_switch_mode = ref true

let wrapper_exec_args = ref []
let add_wrapper_exec_arg s = wrapper_exec_args := s :: !wrapper_exec_args

let from_inside_switch action () =
  set_action action ();
  ocpwin_install_kind := "user";

  let basename =  "ocpwin-version.txt" in
  if not (Sys.file_exists basename) then
    if Sys.file_exists (".." // basename) then
      Unix.chdir ".."
    else begin
      Printf.eprintf "Error: could not find %S in current directory\n%!"
      basename;
      exit 2
    end;
  let dir = Unix.getcwd () in
  let ic = open_in basename in
  let version = input_line ic in
  close_in ic;
  ocpwin_install_version := version;
  ocpwin_install_dir := dir;
  ()

let arg_list =
  Arg.align [
    "-msvc", Arg.Set_string detect_msvc,
    "FLAGS Set flags for MSVC";
    string_of_action MsvcExec, Arg.Tuple [
      Arg.Unit (set_action MsvcExec);
      Arg.Rest add_vc_exec_arg],
    "CMD Call the command CMD";

    "-no-switch", Arg.Clear arg_change_switch,
    " Do not change global switch";
    "-in", Arg.Unit (from_inside_switch InstallSwitch),
    " Configure installation from inside distribution";
    "-un", Arg.Unit (from_inside_switch UninstallSwitch),
    " Configure un-installation from inside distribution";

    string_of_action InstallSwitch, Arg.Tuple [
      Arg.Unit (set_action InstallSwitch);
      Arg.Set_string ocpwin_install_kind;
      Arg.Set_string ocpwin_install_version;
      Arg.Set_string ocpwin_install_dir],
    " VERSION DIR Configure installation";
    string_of_action UninstallSwitch, Arg.Tuple [
      Arg.Unit (set_action UninstallSwitch);
      Arg.Set_string ocpwin_install_kind;
      Arg.Set_string ocpwin_install_version;
      Arg.Set_string ocpwin_install_dir],
    " VERSION DIR Configure un-installation";
    string_of_action ListSwitches, Arg.Unit (set_action ListSwitches),
    " List installed switches";
    string_of_action SetSwitch, Arg.Tuple
      [Arg.Unit (set_action SetSwitch);
       Arg.Set_string current_switch],
    "SWITCH Add switch binaries to global path";
    string_of_action PrintPath, Arg.Unit (set_action PrintPath),
    " Print PATH values";

    "-force-update-wrappers", Arg.String (
      fun s -> arg_force_update_wrappers := Some s),
    "FILENAME Force update ocp-wrapper.exe in all wrapper directories";
    string_of_action MultiSwitch, Arg.Tuple
      [Arg.Unit (set_action MultiSwitch);
       Arg.Bool (fun b -> multi_switch_mode := b)],
    "BOOL Set multi-switch mode";

    string_of_action Wrapper, Arg.Tuple
      [Arg.Unit (set_action Wrapper);
       Arg.Rest add_wrapper_exec_arg],
    "CMD Call command in multi-switch env";

    "-verbose", Arg.Set verbose,
    " Add some debug output";
  ]

let arg_anon s =
  Printf.eprintf "ocpwin-tool: unexpected non-prefixed argument %S\n%!" s;
  exit 2

let arg_usage = "ocpwin-tool [OPTIONS]"

let run_as_ocpwin () =
  Arg.parse arg_list arg_anon arg_usage;
  match !action with
  | None ->
    Printf.eprintf "Error: no action specified\n%!";
    Arg.usage arg_list arg_usage;
    exit 2
  | Some action ->
    match action with
    | MsvcExec ->
      Msvc.detect_visual_studio !detect_msvc;
      Unix.putenv "OCPWIN_MSVC" (
        try
          !detect_msvc ^ ":" ^ (Sys.getenv "OCPWIN_MSVC")
        with Not_found -> !detect_msvc);
      let args = Array.of_list (List.rev !vc_exec_args) in
      OnlyWin32.simulate_exec args.(0) args
    | ListSwitches ->
      list_switches ()
    | InstallSwitch ->
      if !verbose then print_args ();
      install_switch !ocpwin_install_kind !ocpwin_install_version !ocpwin_install_dir
    | UninstallSwitch ->
      uninstall_switch !ocpwin_install_kind !ocpwin_install_version !ocpwin_install_dir
    | SetSwitch ->
      set_switch !current_switch
    | PrintPath ->
      print_PATH ()
    | MultiSwitch ->
      set_multi_switch !multi_switch_mode
    | Wrapper ->
      let args = Array.of_list (List.rev !wrapper_exec_args) in
      call_command_through_wrapper None args

(* If argv[0] is not "ocpwin.exe", we are being used as a direct
   wrapper for "ocamlrun.exe" with the name of the bytecode file as
   argv[0].
*)
let _ =
  if !verbose then print_args ();
  match String.lowercase (Filename.basename (Sys.argv.(0))) with
  | "ocpwin" | "ocpwin.exe" | "ocpwin.asm" -> run_as_ocpwin ()
  | command ->
    call_command_through_wrapper (Some "ocamlrun.exe") Sys.argv
