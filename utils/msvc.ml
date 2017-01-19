(******************************************************************)
(*                                                                *)
(*                    OCaml for Windows                           *)
(*                                                                *)
(*           Copyright: OCamlPro 2014-2015                        *)
(*                                                                *)
(******************************************************************)

(* Auto-detection of MSVC installation.

Supported versions:
* 9.0: Windows SDK and .Net framework 4.5
* 11.0: Visual Studio 2012
* 12.0: Visual Studio 2013
* 14.0: Visual Studio Community 2015

When MSVC is detected:
* We set the PATH, LIBPATH, LIB, INCLUDE env variables accordingly
* We add $OCAMLLIB/msvc-$MSVCVERSION to the load_path, for C stubs libraries
   to take precedence over mingw ones

*)

(* open StringCompat *)

let verbose = ref false
let export = ref false
let cygwin = ref false
let sixtyfourp = ref (Config.system = "win64")

module WinRegistry :sig

  type dwType =
  | REG_SZ
  | REG_EXPAND_SZ

  type rkey_value = string array * string * dwType
  val read_standard_registry_config : rkey_value -> string

end = struct

type hroot =
  | HKEY_CLASSES_ROOT
  | HKEY_CURRENT_CONFIG
  | HKEY_CURRENT_USER
  | HKEY_LOCAL_MACHINE
  | HKEY_USERS

type dwType =
  | REG_SZ
  | REG_EXPAND_SZ

type rkey_value = string array * string * dwType

external read_registry_key :
  hroot ->
    string array ->
        string ->
          (* string to read (result) *)  bytes ->
            (* size read *) int
            = "caml_ocp_win32_read_registry"

let read_registry_key hroot (s2, s3, _) =
  let len = 8192 in
  let s = Bytes.create len in
  let res_len = read_registry_key hroot s2 s3 s in
  if res_len < 0 then raise Not_found;
  Bytes.to_string (Bytes.sub s 0 res_len)

let read_standard_registry_config key =
  try
(*    if !verbose then
       Printf.fprintf stderr "Trying HKCU\\%s...\n%!"
       (string_of_reg_key key); *)
    read_registry_key HKEY_CURRENT_USER key
  with _ ->
(*    if !verbose then
       Printf.fprintf stderr "Trying HKLM\\%s...\n%!"
       (string_of_reg_key key); *)
     read_registry_key HKEY_LOCAL_MACHINE key

(* To avoid Warning 37... *)
let _ = HKEY_USERS
let _ = HKEY_CURRENT_CONFIG
let _ = HKEY_CLASSES_ROOT
end

open WinRegistry


external putenv : string -> string -> int = "caml_ocp_putenv"
let putenv name v =
  if putenv name v <> 0 then Printf.kprintf failwith "putenv(%S) failed" name

    (*
(* For Windows : *)
let windows_sdk_install_dir_key =
  ([| "SOFTWARE"; "Microsoft"; "Microsoft SDKs"; "Windows" |],
   "CurrentInstallFolder",
   REG_SZ)

let windows_visual_studio_dir_key =
 ([| "SOFTWARE"; "Microsoft"; "VisualStudio"; "9.0"; "Setup"; "VC" |],
  "ProductDir",
  REG_SZ )

(* if we are running in 64bits, 32bits keys are in SOFTWARE/Wow6432Node. Since
   VC is a 32bit program, we should try there too. *)
let windows_sdk_install_dir_key64 =
  ([| "SOFTWARE"; "Wow6432Node"; "Microsoft"; "Microsoft SDKs"; "Windows" |],
   "CurrentInstallFolder",
   REG_SZ)
let windows_visual_studio_dir_key64 =
 ([| "SOFTWARE";"Wow6432Node";"Microsoft";"VisualStudio";"9.0";"Setup";"VC" |],
  "ProductDir",
  REG_SZ )
    *)


let path_sep = if Sys.win32 then ";" else ":"

let break_path path = String.split_on_char path_sep.[0] path
let read_path_from_env name =
  ref (try break_path (Sys.getenv name) with _ -> [])
let add_to_path dir path =
  path := dir :: (List.filter ( (<>) dir) !path)
let add_to_path_if_exists dir path =
  if !verbose then
    Printf.eprintf "add_to_path_if_exists %S ?\n%!" dir;
  if Sys.file_exists dir then begin
    path := dir :: (List.filter ( (<>) dir) !path);
    if !verbose then
      Printf.eprintf "add_to_path_if_exists %S: YES !\n%!" dir;
  end else begin
    if !verbose then
      Printf.eprintf "add_to_path_if_exists %S: NO !?\n%!" dir;
  end

let write_path_to_env name path =
  let path = String.concat path_sep !path in
  if !verbose then begin
    Printf.eprintf "+ putenv %s=%S\n%!" name path;
  end;
  putenv name path

let rec add_basenames path names =
  match names with
  | [] -> path
  | "" :: names -> add_basenames path names
  | name :: names -> add_basenames (Filename.concat path name) names


(* From:
c:/Program Files/Microsoft Visual Studio 9.0/VC/bin/vcvars32.bat
c:/Program Files/Microsoft Visual Studio 9.0/VC/bin/vcvarsx86_amd64.bat
*)

 (* remove backslash at end, to have more readable strings, and to fix a problem
    in Filename.dirname in 3.11.2 *)
let no_backslash_at_end dir =
  let len = String.length dir in
  if len > 4 && dir.[len-1] = '\\' then String.sub dir 0 (len-1) else dir

let (//) x y = Printf.sprintf "%s\\%s" x y

let key_SOFTWARE (path, name, kind) =
  (Array.of_list ("SOFTWARE" :: path), name, kind)
(* if we are running in 64bits, 32bits keys are in SOFTWARE/Wow6432Node. Since
   VC is a 32bit program, we should try there too. *)
let key_SOFTWARE_64_32 (path, name, kind) =
  (Array.of_list ("SOFTWARE" ::  "Wow6432Node" ::path), name, kind)

let reg_query key_SOFTWARE ( (path,name,_) as key) =
  let s = WinRegistry.read_standard_registry_config (key_SOFTWARE key) in
  if !verbose then
    Printf.eprintf "ocpwin-msvc: reg %s.%s = %S\n%!"
      (String.concat "/" path) name s;
  s

let reg32_query ( (path,name,_) as key) =
  try reg_query key_SOFTWARE key
  with Not_found ->
    try reg_query key_SOFTWARE_64_32 key
    with Not_found ->
      if !verbose then
        Printf.eprintf "ocpwin-msvc: reg %s.%s not found\n%!"
          (String.concat "/" path) name;
      raise Not_found

  let get_versions_from_dir dir =
    let var = ref "" in
    begin try
            let versions = Sys.readdir dir in
            Array.sort compare versions;
            Array.iter (fun file ->
                var := file;
            ) versions;
      with _ -> () end;
    !var


  let autodetect detect_visual_studio version sixtyfourp =
    if !verbose then
      Printf.eprintf "ocpwin-msvc: auto-detecting MSVC %s\n%!" version;
    try
      let dir = detect_visual_studio sixtyfourp in
      if !verbose then
        Printf.eprintf "ocpwin-msvc: MSVC %s found in %S\n%!" version dir;
      true
    with Not_found ->
      if !verbose then
        Printf.eprintf "ocpwin-msvc: MSVC %s not found\n%!" version;
      false

  type msvc_env = {
    version : string;
    env_PATH : string list ref;
    env_LIB : string list ref;
    env_INCLUDE : string list ref;
    env_LIBPATH : string list ref;
    env_ProgramFiles : string;
    env_ProgramFilesX86 : string;
  }

  let read_MSVC_env version =
    let env_ProgramFiles = try Sys.getenv "ProgramFiles"
      with _ -> "C:/Program Files" in
    let env_ProgramFilesX86 = try Sys.getenv "ProgramFiles(x86)" with _ ->
      env_ProgramFiles in
    {
    version;
    env_PATH = read_path_from_env "PATH";
    env_INCLUDE = read_path_from_env "INCLUDE";
    env_LIB = read_path_from_env "LIB";
    env_LIBPATH = read_path_from_env "LIBPATH";
    env_ProgramFiles; env_ProgramFilesX86;
  }

  let slashify_filename s =
    let s = Bytes.of_string s in
    for i = 0 to Bytes.length s - 1 do
      if Bytes.get s i = '\\' then Bytes.set s i '/'
    done;
    Bytes.to_string s

  let slashify_path path = List.map slashify_filename path

  let cygwin_filename s=
    let s = slashify_filename s in
    let len = String.length s in
    if len > 1 && s.[1] = ':' &&
      (match s.[0] with
        'a'..'z' | 'A'..'Z' -> true
      | _ -> false) then begin
        Printf.sprintf "/cygdrive/%c%s"
          s.[0] (String.sub s 2 (len-2))
      end else s

  let gitbash_filename s =
    let s = slashify_filename s in
    let len = String.length s in
    if len > 1 && s.[1] = ':' &&
      (match s.[0] with
        'a'..'z' | 'A'..'Z' -> true
      | _ -> false) then begin
        Printf.sprintf "/%c%s"
          s.[0] (String.sub s 2 (len-2))
      end else
      s

  let cygwin_path path =
    List.map cygwin_filename path

  let gitbash_path path =
    List.map gitbash_filename path

  let unix_path path = String.concat ":" path
  let windows_path path = String.concat ";" path



  let write_MSVC_env env =
    let { version; env_PATH; env_INCLUDE; env_LIB; env_LIBPATH } = env in

    let ocamllib = Config.standard_library in

    (* All C-stubs have to be compiled for a specific version of
       MSVC. For each supported version by OcpWin, we store these stub
       libraries into the following directory. *)
    let msvc_dir =
      ocamllib // (match version with
      | "mingw" -> "mingw"
      | version -> "msvc-" ^ version) in
    add_to_path msvc_dir env_PATH;
    add_to_path msvc_dir env_LIBPATH;
    add_to_path msvc_dir env_LIB;

(* We must do the following, because ocamlopt lookup libasmrun.a in
   the load_path, and will explicit a wrong libasmrun.a otherwise. So,
   we add the MSVC specific dir first, so that it explicits the correct
   location for libasmrun.a. Setting Config.load_path fails because
   it is recomputed too often. *)
    Clflags.include_dirs := msvc_dir :: !Clflags.include_dirs;

    if !export then begin
      let filename = Printf.sprintf "vcvars%s.sh"
        (if !sixtyfourp then "64" else "32")
      in
      Printf.eprintf "Exporting MSVC variables to %S\n%!" filename;
      let export_var oc var new_path =
        let old_path = read_path_from_env var in
        let rec shorten_path rev_path rev_old_path =
          match rev_path, rev_old_path with
          | s1 :: rev_path, s2 :: rev_old_path
            when s1 = s2 -> shorten_path rev_path rev_old_path
          | _ -> rev_path
        in
        let rev_path = shorten_path (List.rev !new_path) (List.rev !old_path) in
        if rev_path <> [] then
          let path = List.rev (("$"^var) :: rev_path) in
          let path =
            match !cygwin, var with
            | true, "PATH" ->
              unix_path (cygwin_path path)
            | true, _ ->
              windows_path (slashify_path path)
            | false, _ ->
              unix_path (gitbash_path path)
          in
          Printf.fprintf oc "export %s=\"%s\"\n" var path
      in
      let oc = open_out filename in
      Printf.fprintf oc "# auto-detected MSVC %s variables\n" version;
      export_var oc "PATH" env_PATH;
      export_var oc "INCLUDE" env_INCLUDE;
      export_var oc "LIB" env_LIB;
      export_var oc "LIBPATH" env_LIBPATH;
      close_out oc;

    end;

    write_path_to_env "PATH" env_PATH;
    write_path_to_env "INCLUDE" env_INCLUDE;
    write_path_to_env "LIB" env_LIB;
    write_path_to_env "LIBPATH" env_LIBPATH;
    ()


module VS_9_0 = struct

(* Windows 7 SDK:
   Minimal Selection:
   * Developer Tools:
      * Windows Headers and Libraries
      * Visual C++ Compilers
      * Windows Development Tools
        * Win32 Development Tools
      * Debugging Tools for Windows
*)

  let windows_sdk_install_dir_key =
    ([ "Microsoft"; "Microsoft SDKs"; "Windows" ],
     "CurrentInstallFolder",
     REG_SZ)

  let windows_visual_studio_dir_key =
    ([ "Microsoft"; "VisualStudio"; "9.0"; "Setup"; "VC" ],
     "ProductDir",
     REG_SZ )



  let detect_visual_studio sixtyfourp =
    let var_VCINSTALLDIR =
      no_backslash_at_end (reg32_query  windows_visual_studio_dir_key) in
    let var_WindowsSdKDir =
      try Some (
        no_backslash_at_end (reg32_query windows_sdk_install_dir_key)
      ) with _ -> None
    in
    (* Printf.fprintf stderr "VC_INSTALL_DIR=%s\n%!" vc_install_dir;
       Printf.fprintf stderr "SDK_INSTALL_DIR=%s\n%!"
       sdk_install_dir; *)
    (* This is for
       Setting environment for using Microsoft Visual Studio 2008 x86 tools.
    *)

    let msvc_env = read_MSVC_env "9.0" in
    let { env_PATH; env_INCLUDE; env_LIB; env_LIBPATH } = msvc_env in


    let var_VSINSTALLDIR = Filename.dirname var_VCINSTALLDIR in
    let dev_env_dir = var_VSINSTALLDIR // "Common7" // "IDE"  in
    let maybe_amd64 s = if sixtyfourp then s // "amd64" else s in
    let maybe_x64 s = if sixtyfourp then s // "x64" else s in

    begin match var_WindowsSdKDir with
    | None -> ()
    | Some var_WindowsSdKDir ->
      add_to_path_if_exists (var_WindowsSdKDir // "bin" )  env_PATH;
      add_to_path_if_exists (var_WindowsSdKDir // "win64" // "x64" )  env_PATH;
      add_to_path_if_exists (var_WindowsSdKDir // maybe_x64 "bin" )  env_PATH;
      add_to_path_if_exists (var_WindowsSdKDir // "include" ) env_INCLUDE;
      add_to_path_if_exists (var_WindowsSdKDir // maybe_amd64 "lib") env_LIB;
      add_to_path_if_exists (var_WindowsSdKDir // maybe_x64 "lib") env_LIB;
    end;

    List.iter (fun dir ->
      add_to_path dir env_PATH
    ) [
      var_VCINSTALLDIR // "vcpackages";
      var_VSINSTALLDIR // "Common7" // "IDE";
      var_VSINSTALLDIR // "Common7" // "Tools" // "bin";
      var_VSINSTALLDIR // "Common7" // "Tools";
      (* %FrameworkDir%\%FrameworkVersion% *)
      (* %FrameworkDir%\%Framework35Version% *)
      var_VCINSTALLDIR // maybe_amd64 "bin";
      dev_env_dir;
    ];

    add_to_path (var_VCINSTALLDIR // "include" ) env_INCLUDE;
    add_to_path (var_VCINSTALLDIR // "atlmfc" // "include" ) env_INCLUDE;

    add_to_path (var_VCINSTALLDIR // maybe_amd64 "lib") env_LIB;
    add_to_path (var_VCINSTALLDIR // "atlmfc" // maybe_amd64 "lib") env_LIB;

    add_to_path (var_VCINSTALLDIR // maybe_amd64 "lib") env_LIBPATH;
    add_to_path (var_VCINSTALLDIR // "atlmfc" // maybe_amd64 "lib") env_LIBPATH;
    (* %FrameworkDir%\%FrameworkVersion% *)
    (*  %FrameworkDir%\%Framework35Version% *)

    write_MSVC_env msvc_env;

    var_VCINSTALLDIR

  let detect_visual_studio =
    autodetect detect_visual_studio "9.0"

end

module VS_11_0 = struct
  (* Visual Studio 2012, ISO *)

  let reg_WindowsSdkDir_8_0 =
    ( ["Microsoft";"Microsoft SDKs";"Windows";"v8.0"],
      "InstallationFolder",
      REG_SZ )
  let reg_WindowsSdkDir_8_0a =
    ( ["Microsoft";"Microsoft SDKs";"Windows";"v8.0a"],
      "InstallationFolder",
      REG_SZ )
  let reg_WindowsSdkDir_35 =
    ( ["Microsoft";"Microsoft SDKs";"Windows";"v8.0A";"WinSDK-NetFx35Tools"],
      "InstallationFolder",
      REG_SZ )
  let reg_VSINSTALLDIR =
    (["Microsoft";"VisualStudio";"SxS";"VS7"],
     "11.0", REG_SZ)
  let reg_VCINSTALLDIR =
    (["Microsoft";"VisualStudio";"SxS";"VC7"],
     "11.0", REG_SZ)

  let reg_FrameworkDir64 =
    ( [ "Microsoft";"VisualStudio";"SxS";"VC7"], "FrameworkDir64", REG_SZ )
  let reg_FrameworkDir32 =
    ( [ "Microsoft";"VisualStudio";"SxS";"VC7"], "FrameworkDir32", REG_SZ )
  let reg_FrameworkVer64 =
    ( [ "Microsoft";"VisualStudio";"SxS";"VC7"], "FrameworkVer64", REG_SZ )
  let reg_FrameworkVer32 =
    ( [ "Microsoft";"VisualStudio";"SxS";"VC7"], "FrameworkVer32", REG_SZ )

  let fun_GetFrameworkDir64 () =
    reg32_query reg_FrameworkDir64
  let fun_GetFrameworkDir32 () =
    reg32_query reg_FrameworkDir32
  let fun_GetFrameworkVer64 () =
    reg32_query reg_FrameworkVer64
  let fun_GetFrameworkVer32 () =
    reg32_query reg_FrameworkVer32

  let bat_vcvarsqueryregistry sixtyfourp =
    let var_WindowsSdkDir =
      try
        Some (reg32_query reg_WindowsSdkDir_8_0)
      with Not_found -> None
    in
    let var_WindowsSdkDir_old =
        try
          Some (reg32_query reg_WindowsSdkDir_8_0a)
        with Not_found -> None
    in
    let var_WindowsSdkDir_35 =
      try
        Some (reg32_query reg_WindowsSdkDir_35)
      with Not_found -> None
    in
    let var_VSINSTALLDIR =
      reg32_query reg_VSINSTALLDIR
    in
    let var_VCINSTALLDIR =
      reg32_query reg_VCINSTALLDIR
    in

    let var_FrameworkDir =
      if sixtyfourp then
fun_GetFrameworkDir64 ()
      else
fun_GetFrameworkDir32 ();
    in
    let var_FrameworkVersion =
      if sixtyfourp then
fun_GetFrameworkVer64 ()
      else
fun_GetFrameworkVer32 ()
    in
    let var_Framework35Version = "v3.5"
    in
(*
:GetExtensionSdkDir
@set ExtensionSdkDir=
@if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs\Microsoft.VCLibs\11.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs
@if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs\Microsoft.VCLibs\11.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs
*)

    (*
:GetFSharpInstallDirHelper32
@for /F "tokens=1,2*" %%i in ('reg query "%1\SOFTWARE\Microsoft\VisualStudio\11.0\Setup\F#" /v "ProductDir"') DO (
@if "%%i"=="ProductDir" (
@SET "FSHARPINSTALLDIR=%%k"
)
)
    *)

    (var_VSINSTALLDIR, var_VCINSTALLDIR,
     var_FrameworkDir, var_FrameworkVersion,
     var_Framework35Version,
     var_WindowsSdkDir, var_WindowsSdkDir_old, var_WindowsSdkDir_35
    )

  let detect_visual_studio sixtyfourp =

    let arch = if sixtyfourp then "x64" else "x86" in
    let maybe_amd64 s = if sixtyfourp then s // "amd64" else s in

    (* I think this is only necessary to use the script
       vcvarsqueryregistry.bat. Since we don't use it, we can just get
       rid of this line ?
    let var_VS140COMNTOOLS = fun_GetVSCommonToolsDir () in
    *)

    let (var_VSINSTALLDIR, var_VCINSTALLDIR,
         var_FrameworkDir, var_FrameworkVersion,
         var_Framework35Version,
         var_WindowsSdkDir, var_WindowsSdkDir_old, var_WindowsSdkDir_35
         ) =
      bat_vcvarsqueryregistry sixtyfourp in

    let msvc_env = read_MSVC_env "11.0" in
    let { env_PATH; env_INCLUDE; env_LIB; env_LIBPATH;
          env_ProgramFiles; env_ProgramFilesX86;
        } = msvc_env in

    begin match var_WindowsSdkDir_old with
    | None -> ()
    | Some var_WindowsSdkDir_old ->
      begin match var_WindowsSdkDir_35 with
      | None -> ()
      | Some var_WindowsSdkDir_35 ->
        add_to_path var_WindowsSdkDir_35 env_PATH
      end;
      add_to_path
        (add_basenames var_WindowsSdkDir_old ["bin";"NETFX 4.0 Tools"])
        env_PATH;
    end;

    begin match var_WindowsSdkDir with
    | None -> ()
    | Some var_WindowsSdkDir ->
      add_to_path (add_basenames var_WindowsSdkDir [ "bin";"x86" ]) env_PATH;
      List.iter (fun s -> add_to_path s env_INCLUDE) [
        add_basenames var_WindowsSdkDir ["include";"shared"];
        add_basenames var_WindowsSdkDir ["include";"um"];
        add_basenames var_WindowsSdkDir ["include";"winrt"];
      ];
      add_to_path (
        add_basenames var_WindowsSdkDir [
          "lib";"win8";"um"; arch ]) env_LIB;
      List.iter (fun s -> add_to_path s env_LIBPATH) [
        add_basenames var_WindowsSdkDir
          ["References";"CommonConfiguration";"Neutral"];
        (*
%ExtensionSDKDir%\Microsoft.VCLibs\11.0\References\CommonConfiguration\neutral
        *)
      ];
    end;

(* Root of Visual Studio IDE installed files *)
    let var_DevEnvDir = add_basenames var_VSINSTALLDIR ["Common7";"IDE"] in

    List.iter (fun s -> add_to_path_if_exists s env_PATH) [
      add_basenames var_VSINSTALLDIR ["Team Tools";"Performance Tools"];
      add_basenames env_ProgramFiles ["HTML Help Workshop"];
      add_basenames env_ProgramFilesX86 ["HTML Help Workshop"];
    ];
    List.iter (fun s -> add_to_path s env_PATH) [
      add_basenames var_VCINSTALLDIR ["VCPackages"];
      add_basenames var_FrameworkDir [var_Framework35Version];
      add_basenames var_FrameworkDir [var_FrameworkVersion];
      add_basenames var_VSINSTALLDIR ["Common7";"Tools"];
      add_basenames var_VCINSTALLDIR ["BIN"];
    ];
    if sixtyfourp then
      add_to_path (add_basenames var_VCINSTALLDIR ["BIN";"x86_amd64"]) env_PATH;
    add_to_path var_DevEnvDir env_PATH;

(*
@if not "%FSHARPINSTALLDIR%" == "" (
@set "PATH=%FSHARPINSTALLDIR%;%PATH%"
)
*)
    (*
@if exist "%DevEnvDir%CommonExtensions\Microsoft\TestWindow" (
@set "PATH=%DevEnvDir%CommonExtensions\Microsoft\TestWindow;%PATH%"
)
    *)

    add_to_path_if_exists
      (add_basenames var_VCINSTALLDIR ["ATLMFC";"INCLUDE"]) env_INCLUDE;
    add_to_path (add_basenames var_VCINSTALLDIR ["INCLUDE"]) env_INCLUDE;

    add_to_path_if_exists
      (add_basenames var_VCINSTALLDIR ["ATLMFC";maybe_amd64 "LIB"]) env_LIB;
    add_to_path
      (add_basenames var_VCINSTALLDIR [maybe_amd64 "LIB"]) env_LIB;

    add_to_path_if_exists
      (add_basenames var_VCINSTALLDIR ["ATLMFC";maybe_amd64 "LIB"]) env_LIBPATH;
    List.iter (fun s -> add_to_path s env_LIBPATH) [
      add_basenames var_VCINSTALLDIR [maybe_amd64 "LIB"];
      add_basenames var_FrameworkDir [var_Framework35Version ];
      add_basenames var_FrameworkDir [var_FrameworkVersion ];
      (* weird ?
      add_basenames var_FrameworkDir64 [ var_Framework35Version ];
      add_basenames var_FrameworkDir64 [ var_FrameworkVersion ];
      *)
    ];

(*
:GetVSCommonToolsDirHelper32
@for /F "tokens=1,2*" %%i in ('reg query "%1\SOFTWARE\Microsoft\VisualStudio\SxS\VS7" /v "11.0"') DO (
@if "%%i"=="11.0" (
@SET "VS110COMNTOOLS=%%k"
)
)
*)
    write_MSVC_env msvc_env;
    var_VCINSTALLDIR

  let detect_visual_studio = autodetect detect_visual_studio "11.0"

end

module VS_12_0 = struct


  let reg_WindowsSdkDir_v8_1 =
    ( ["Microsoft";"Microsoft SDKs";"Windows";"v8.1"],
      "InstallationFolder",
      REG_SZ )
  let reg_VSINSTALLDIR =
    (["Microsoft";"VisualStudio";"SxS";"VS7"],
     "12.0", REG_SZ)
  let reg_VCINSTALLDIR =
    (["Microsoft";"VisualStudio";"SxS";"VC7"],
     "12.0", REG_SZ)


  let bat_vcvarsqueryregistry sixtyfourp =
    let var_WindowsSdkDir =
      try
        Some (reg32_query reg_WindowsSdkDir_v8_1)
      with Not_found -> None
    in
    let var_VSINSTALLDIR =
      reg32_query reg_VSINSTALLDIR
    in
    let var_VCINSTALLDIR =
      reg32_query reg_VCINSTALLDIR
    in

    let var_FrameworkDir =
      if sixtyfourp then
        VS_11_0.fun_GetFrameworkDir64 ()
      else
        VS_11_0.fun_GetFrameworkDir32 ();
    in
    let var_FrameworkVersion =
      if sixtyfourp then
        VS_11_0.fun_GetFrameworkVer64 ()
      else
        VS_11_0.fun_GetFrameworkVer32 ()
    in
    (*
      :GetExtensionSdkDir
      @set ExtensionSdkDir=
      @if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs\Microsoft.VCLibs\11.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs
      @if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs\Microsoft.VCLibs\11.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\ExtensionSDKs
    *)

    (*
      :GetFSharpInstallDirHelper32
      @for /F "tokens=1,2*" %%i in ('reg query "%1\SOFTWARE\Microsoft\VisualStudio\11.0\Setup\F#" /v "ProductDir"') DO (
      @if "%%i"=="ProductDir" (
      @SET "FSHARPINSTALLDIR=%%k"
      )
      )
    *)

    (var_VSINSTALLDIR, var_VCINSTALLDIR,
     var_FrameworkDir, var_FrameworkVersion,
     var_WindowsSdkDir
    )

  let detect_visual_studio sixtyfourp =

    let arch = if sixtyfourp then "x64" else "x86" in
    let maybe_amd64 s = if sixtyfourp then s // "amd64" else s in
    let maybe_x64 s = if sixtyfourp then s // "x64" else s in

    let (var_VSINSTALLDIR, var_VCINSTALLDIR,
         var_FrameworkDir, var_FrameworkVersion,
         var_WindowsSdkDir
    ) =
      bat_vcvarsqueryregistry sixtyfourp in
    let var_Framework40Version= "v4.0" in

    let msvc_env = read_MSVC_env "12.0" in
    let { env_PATH; env_INCLUDE; env_LIB; env_LIBPATH;
          env_ProgramFiles; env_ProgramFilesX86;
        } = msvc_env in


    (*
      @if not "%WindowsSDK_ExecutablePath_x64%" == "" (
      @set "PATH=%WindowsSDK_ExecutablePath_x64%;%PATH%"
      )
    *)

    begin match var_WindowsSdkDir with
    | None -> ()
    | Some var_WindowsSdkDir ->
      add_to_path (add_basenames var_WindowsSdkDir [ "bin"; "x86" ]) env_PATH;

      List.iter (fun s -> add_to_path s env_INCLUDE) [
        add_basenames var_WindowsSdkDir ["include";"shared"];
        add_basenames var_WindowsSdkDir ["include"; "um"];
        add_basenames var_WindowsSdkDir ["include";"winrt"];
      ];
      add_to_path (
        add_basenames var_WindowsSdkDir [
          "lib";"winv6.3";"um"; arch ]) env_LIB;
      List.iter (fun s -> add_to_path s env_LIBPATH) [
        add_basenames var_WindowsSdkDir
          ["References";"CommonConfiguration";"Neutral"];
      (*
        %ExtensionSDKDir%\Microsoft.VCLibs\12.0\References\CommonConfiguration\neutral
      *)
      ];
    end;

    List.iter (fun s -> add_to_path_if_exists s env_PATH)
      (
        [
          add_basenames var_VSINSTALLDIR ["Team Tools";"Performance Tools"];
          add_basenames var_VSINSTALLDIR ["Team Tools"; maybe_x64 "Performance Tools"];
          add_basenames env_ProgramFiles ["HTML Help Workshop"];
          add_basenames env_ProgramFilesX86 ["HTML Help Workshop"];

          add_basenames var_VSINSTALLDIR ["Common7";"Tools" ];
          add_basenames var_VSINSTALLDIR ["Common7";"IDE" ];
          add_basenames var_VCINSTALLDIR [ "VCPackages" ];
          add_basenames var_FrameworkDir [var_Framework40Version];
          add_basenames var_FrameworkDir [var_FrameworkVersion];
          add_basenames var_VCINSTALLDIR [maybe_amd64 "BIN"];
          add_basenames var_VCINSTALLDIR ["bin" ];
        ] @ (
          if sixtyfourp then
            [add_basenames var_VCINSTALLDIR ["bin"; "x86_amd64" ]]
          else []) @
          [
            add_basenames env_ProgramFiles ["MSBuild";"12.0";"bin"];
            add_basenames env_ProgramFiles ["MSBuild";"12.0";maybe_amd64 "bin"];
            add_basenames env_ProgramFilesX86 ["MSBuild";"12.0"; "bin" ];
            add_basenames env_ProgramFilesX86 ["MSBuild";"12.0"; maybe_amd64 "bin" ];

            add_basenames var_VSINSTALLDIR ["Common7";"IDE";"CommonExtensions";"Microsoft";"TestWindow"];
          ]);

    add_to_path_if_exists
      (add_basenames var_VCINSTALLDIR ["ATLMFC";"INCLUDE"]) env_INCLUDE;
    add_to_path (add_basenames var_VCINSTALLDIR ["INCLUDE"]) env_INCLUDE;

    add_to_path_if_exists
      (add_basenames var_VCINSTALLDIR ["ATLMFC";maybe_amd64 "LIB"]) env_LIB;
    add_to_path
      (add_basenames var_VCINSTALLDIR [maybe_amd64 "LIB"]) env_LIB;

    add_to_path_if_exists
      (add_basenames var_VCINSTALLDIR ["ATLMFC";maybe_amd64 "LIB"]) env_LIBPATH;
    List.iter (fun s -> add_to_path s env_LIBPATH) [
      add_basenames var_VCINSTALLDIR [maybe_amd64 "LIB"];
      add_basenames var_FrameworkDir [var_Framework40Version ];
      add_basenames var_FrameworkDir [var_FrameworkVersion ];
    ];

    (*
      :GetVSCommonToolsDirHelper32
      @for /F "tokens=1,2*" %%i in ('reg query "%1\SOFTWARE\Microsoft\VisualStudio\SxS\VS7" /v "11.0"') DO (
      @if "%%i"=="11.0" (
      @SET "VS110COMNTOOLS=%%k"
      )
      )
    *)
    write_MSVC_env msvc_env;
    var_VCINSTALLDIR


  let detect_visual_studio = autodetect detect_visual_studio "12.0"

end


module VS_14_0 = struct
(* Visual Studio Express, Community 2015:

* 2 Selected Components:
  Programming Languages
   -> Visual C++
     -> Common Tools for Visual C++ 2015
  Windows and Web Development
   -> Universal Windows App Development Tools
     -> Tools (1.1.1) and Windows SDK (10.0.10240)
*)

  (*******************************************************************)
  (* equivalent to vcvarsqueryregistry.bat *)

  let reg_WindowsSdkDir_v10_0 =
    ( ["Microsoft";"Microsoft SDKs";"Windows";"v10.0"],
      "InstallationFolder",
      REG_SZ )

  let fun_GetWindowsSdkDir () =
    (*  Get Windows 10 SDK installed folder *)
    try
      let var_WindowsSdkDir = reg32_query reg_WindowsSdkDir_v10_0 in
      if !verbose then
        Printf.eprintf "WindowsSdkDir = %S\n%!" var_WindowsSdkDir;
      (* Get windows 10 sdk version number *)
      let var_WindowsSDKVersion = get_versions_from_dir
        (add_basenames var_WindowsSdkDir ["include"]) in
      if !verbose then
        Printf.eprintf "WindowsSdkVersion = %S\n%!" var_WindowsSDKVersion;

      let var_WindowsLibPath = String.concat ";" [
        add_basenames var_WindowsSdkDir ["UnionMetadata" ];
        add_basenames var_WindowsSdkDir ["References" ];
        add_basenames var_WindowsSdkDir
          ["References";"Windows.Foundation.UniversalApiContract";"1.0.0.0" ];
        add_basenames var_WindowsSdkDir
          ["References";"Windows.Foundation.FoundationContract";"1.0.0.0" ];
        add_basenames var_WindowsSdkDir
          ["References";"Windows.Networking.Connectivity.WwanContract";"1.0.0.0" ]
      ] in
      let var_ExtensionSDKDir = "" in (* TODO *)
      let var_WindowsSDKLibVersion = var_WindowsSDKVersion in
      Some (var_WindowsSdkDir, var_WindowsSDKVersion,
            var_WindowsSDKLibVersion,
            var_WindowsLibPath, var_ExtensionSDKDir)
    with Not_found ->
      try
        let var_WindowsSdkDir = reg32_query VS_12_0.reg_WindowsSdkDir_v8_1 in
        let var_WindowsSDKVersion = "" in
        let var_WindowsLibPath =
          add_basenames var_WindowsSdkDir
            ["References";"CommonConfiguration";"Neutral" ] in
        let var_WindowsSDKLibVersion = "winv6.3" (* TODO: check that removing the \\ is ok *) in
        let var_ExtensionSDKDir = "" in (* TODO *)
        Some (var_WindowsSdkDir, var_WindowsSDKVersion,
              var_WindowsSDKLibVersion,
              var_WindowsLibPath, var_ExtensionSDKDir)
      with Not_found ->
        None

  let reg_UniversalCRTSdkDir =
    ([ "Microsoft";"Windows Kits";"Installed Roots"],
       "KitsRoot10",REG_SZ)

  let fun_GetUniversalCRTSdkDir () =
    try
      let var_UniversalCRTSdkDir = reg32_query reg_UniversalCRTSdkDir in
      let var_UCRTVersion = get_versions_from_dir
        (add_basenames var_UniversalCRTSdkDir ["include"]) in
      Some (var_UniversalCRTSdkDir, var_UCRTVersion)
    with Not_found -> None

(*
:GetExtensionSdkDir
@set ExtensionSdkDir=

@REM Windows 8.1 Extension SDK
@if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\ExtensionSDKs\Microsoft.VCLibs\14.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles%\Microsoft SDKs\Windows\v8.1\ExtensionSDKs
@if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\ExtensionSDKs\Microsoft.VCLibs\14.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\ExtensionSDKs

@REM Windows 10 Extension SDK, this will replace the Windows 8.1 "ExtensionSdkDir" if Windows 10 SDK is installed
@if exist "%ProgramFiles%\Microsoft SDKs\Windows Kits\10\ExtensionSDKs\Microsoft.VCLibs\14.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles%\Microsoft SDKs\Windows Kits\10\ExtensionSDKs
@if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows Kits\10\ExtensionSDKs\Microsoft.VCLibs\14.0\SDKManifest.xml" set ExtensionSdkDir=%ProgramFiles(x86)%\Microsoft SDKs\Windows Kits\10\ExtensionSDKs
 *)

  let reg_VSInstallDir =
    ( ["Microsoft";"VisualStudio";"SxS";"VS7"], "14.0", REG_SZ )
  let reg_VCInstallDir =
    ( ["Microsoft";"VisualStudio";"SxS";"VC7"], "14.0", REG_SZ )

  let fun_GetVSInstallDir () =
    reg32_query reg_VSInstallDir
  let fun_GetVCInstallDir () =
    reg32_query reg_VCInstallDir

  let bat_vcvarsqueryregistry sixtyfourp =
    let var_WindowsSdkDir = fun_GetWindowsSdkDir () in
    (*
      fun_GetWindowsSdkExecutablePath32 ();
      fun_GetWindowsSdkExecutablePath64 ();
      fun_GetExtensionSdkDir (); *)
    let var_VSInstallDir = fun_GetVSInstallDir () in
    let var_VCInstallDir = fun_GetVCInstallDir () in
    (* fun_GetFSharpInstallDir (); *)
    let var_UniversalCRTSdkDir = fun_GetUniversalCRTSdkDir () in

    let var_FrameworkDir =
      if sixtyfourp then
        VS_11_0.fun_GetFrameworkDir64 ()
      else
        VS_11_0.fun_GetFrameworkDir32 ();
    in
    let var_FrameworkVersion =
      if sixtyfourp then
        VS_11_0.fun_GetFrameworkVer64 ()
      else
        VS_11_0.fun_GetFrameworkVer32 ()
    in
    let var_Framework40Version = "v4.0" in
    (*    let var_VisualStudioVersion = "14.0" in *)
    (var_VSInstallDir, var_VCInstallDir,
     var_FrameworkDir, var_FrameworkVersion,
     var_Framework40Version,
     var_WindowsSdkDir,
     var_UniversalCRTSdkDir)

  (*******************************************************************)
  (* equivalent to vcvars32.bat *)
(* Note: the .bat script reads in HKLM first, and then in HKCU. Machine
   settings are used in priority over user settings ! *)



(*
  let key_VSCommonToolsDirHelper =
    (["Microsoft"; "VisualStudio";"SxS";"VS7"], "14.0", REG_SZ)
*)

      (*
  let fun_GetVSCommonToolsDir () =
    let var_VS140COMNTOOLS = reg32_query  key_VSCommonToolsDirHelper in
    add_basenames var_VS140COMNTOOLS ["Common7"; "Tools"]
      *)

  let detect_visual_studio sixtyfourp =

    let arch = if sixtyfourp then "x64" else "x86" in
    let maybe_amd64 s = if sixtyfourp then s // "amd64" else s in

    (* I think this is only necessary to use the script
       vcvarsqueryregistry.bat. Since we don't use it, we can just get
       rid of this line ?
    let var_VS140COMNTOOLS = fun_GetVSCommonToolsDir () in
    *)

    let (var_VSINSTALLDIR, var_VCINSTALLDIR,
         var_FrameworkDir, var_FrameworkVersion,
         var_Framework40Version,
         var_WindowsSdkDir,
         var_UniversalCRTSdkDir) =
      bat_vcvarsqueryregistry sixtyfourp in


    let msvc_env = read_MSVC_env "14.0" in
    let { env_PATH; env_INCLUDE; env_LIB; env_LIBPATH;
          env_ProgramFiles; env_ProgramFilesX86; } = msvc_env in


      (*
        @if not "%WindowsSDK_ExecutablePath_x86%" == "" @set PATH=%WindowsSDK_ExecutablePath_x86%;%PATH%
      *)

    begin match var_WindowsSdkDir with
    | None -> ()
    | Some (var_WindowsSdkDir, var_WindowsSDKVersion,
            var_WindowsSDKLibVersion,
            var_WindowsLibPath, var_ExtensionSDKDir) ->

        (* Set Windows SDK include/lib path *)
      add_to_path (add_basenames var_WindowsSdkDir ["bin";arch]) env_PATH;
      List.iter (fun s -> add_to_path s env_INCLUDE)
        [
          add_basenames var_WindowsSdkDir
            ["include";var_WindowsSDKVersion; "winrt"];
          add_basenames var_WindowsSdkDir
            ["include";var_WindowsSDKVersion; "um"];
          add_basenames var_WindowsSdkDir
            ["include"; var_WindowsSDKVersion; "shared"]; (* to check ! *)
        ];
      add_to_path (add_basenames var_WindowsSdkDir
                     ["lib"; var_WindowsSDKLibVersion; "um"; arch]) env_LIB;
      List.iter (fun s -> add_to_path s env_LIBPATH) [
        add_basenames var_ExtensionSDKDir
          ["Microsoft.VCLibs";"14.0";"References";
           "CommonConfiguration";"neutral"];
        var_WindowsLibPath
      ];
    end;
    (*
      @REM Set NETFXSDK include/lib path
      @if not "%NETFXSDKDir%" == "" @set INCLUDE=%NETFXSDKDir%include\um;%INCLUDE%
      @if not "%NETFXSDKDir%" == "" @set LIB=%NETFXSDKDir%lib\um\x86;%LIB%
    *)

      (* Set UniversalCRT include/lib path, the default is the latest installed version. *)
    begin match var_UniversalCRTSdkDir with
    | None -> ()
    | Some (var_UniversalCRTSdkDir, var_UCRTVersion) ->
      add_to_path
        (add_basenames var_UniversalCRTSdkDir
           ["include"; var_UCRTVersion; "ucrt"])
        env_INCLUDE;
      add_to_path
        (add_basenames var_UniversalCRTSdkDir
           ["lib";var_UCRTVersion;"ucrt";arch]) env_LIB;
    end;


    (*  Root of Visual Studio IDE installed files. *)
    (*  *)
    let var_DevEnvDir= add_basenames var_VSINSTALLDIR ["Common7";"IDE"] in
    (*  PATH *)
    (*  ---- *)
    List.iter (fun dir ->
      add_to_path_if_exists dir env_PATH) [

      add_basenames var_VSINSTALLDIR ["Team Tools";"Performance Tools"];
      add_basenames var_VSINSTALLDIR ["Team Tools";"Performance Tools"; arch];

      add_basenames env_ProgramFiles ["HTML Help Workshop"];
      add_basenames env_ProgramFilesX86 ["HTML Help Workshop"];
      add_basenames var_VCINSTALLDIR ["VCPackages"];
      add_basenames var_FrameworkDir [var_Framework40Version];
      add_basenames var_FrameworkDir [var_FrameworkVersion];
      add_basenames var_VSINSTALLDIR ["Common7";"Tools"];
      add_basenames var_VCINSTALLDIR [ maybe_amd64 "BIN"];
      var_DevEnvDir;

      (*  Add path to MSBuild Binaries *)
      add_basenames env_ProgramFiles [ "MSBuild";"14.0";maybe_amd64 "bin"];
      add_basenames env_ProgramFilesX86 [ "MSBuild";"14.0"; maybe_amd64 "bin"];

      add_basenames var_VSINSTALLDIR ["VSTSDB";"Deploy"];

      (*      @if not "%FSHARPINSTALLDIR%" == "" @set PATH=%FSHARPINSTALLDIR%;%PATH% *)

      add_basenames var_DevEnvDir
        [ "CommonExtensions";"Microsoft";"TestWindow" ];
    ];

    (*  INCLUDE *)
    List.iter (fun dir ->
      add_to_path_if_exists dir env_INCLUDE) [

      add_basenames var_VCINSTALLDIR [ "ATLMFC";"INCLUDE"];
      add_basenames var_VCINSTALLDIR ["INCLUDE"];
    ];

(*  LIB *)
    List.iter (fun dir ->
      add_to_path_if_exists dir env_LIB) [

      add_basenames var_VCINSTALLDIR [ "ATLMFC";maybe_amd64 "LIB" ];
      add_basenames var_VCINSTALLDIR [ maybe_amd64 "LIB" ];

      (*  :setstorelib
          @if exist "%VCINSTALLDIR%LIB\store" set LIB=%VCINSTALLDIR%LIB\store;%LIB% *)
    ];

        (*  LIBPATH *)
    List.iter (fun dir ->
      add_to_path_if_exists dir env_LIBPATH) [

        add_basenames var_VCINSTALLDIR [ "ATLMFC";maybe_amd64 "LIB"];
        add_basenames var_VCINSTALLDIR [ maybe_amd64 "LIB" ];
(*
        :setstorelibpath
        @if exist "%VCINSTALLDIR%LIB\store" set LIBPATH=%VCINSTALLDIR%LIB\store;%LIBPATH%
*)

        add_basenames var_FrameworkDir [ var_Framework40Version ];
        add_basenames var_FrameworkDir [ var_FrameworkVersion ];
    ];
    write_MSVC_env msvc_env;
    var_VCINSTALLDIR

  let detect_visual_studio = autodetect detect_visual_studio "14.0"
end


type detected = NotTried | Found | NotFound
let detected = ref NotTried

let auto_detect_visual_studio sixtyfourp =
  VS_9_0.detect_visual_studio sixtyfourp ||
    VS_11_0.detect_visual_studio sixtyfourp ||
    VS_12_0.detect_visual_studio sixtyfourp ||
    VS_14_0.detect_visual_studio sixtyfourp

let detect_visual_studio detect_msvc =
  match !detected with
  | NotFound -> ()
  | Found -> ()
  | NotTried ->
    let s =
     Printf.sprintf "%s:%s:%s"
       detect_msvc (try Sys.getenv "OCPWIN_MSVC" with _ -> "")
       !Config.detect_msvc
    in
    let flags = String.split_on_char ':' s in
    let rec iter flags =
      match flags with
        [] ->
          begin match !detected with
          | NotFound -> ()
          | _ ->
            detected := (
              if auto_detect_visual_studio !sixtyfourp then
                Found
              else NotFound
            );
          end;
          begin match !detected with
          | NotFound ->
            (* The user specified a version that we couldn't find *)
            Printf.eprintf "Error: could not detect any version of MSVC\n";
            Printf.eprintf "  OCPWIN_MSVC=%S\n%!" s;
            exit 2
          | _ -> ()
          end
      | flag :: flags ->
        detected := NotFound; (* a new attempt ! *)
        begin match flag with
        | "no" | "false" | "mingw" -> detected := Found
        | "verbose" ->
          verbose := true;
          detected := NotTried;  (* not an attempt ! *)
          iter flags
        | "export" ->
          export := true;
          detected := NotTried; (* not an attempt ! *)
          iter flags
        | "x86" ->
          sixtyfourp := false;
          detected := NotTried; (* not an attempt ! *)
          iter flags
        | "x64" ->
          sixtyfourp := true;
          detected := NotTried; (* not an attempt ! *)
          iter flags
        | "cygwin" ->
          if not !export then
            Printf.eprintf
              "Warning: \"cygwin\" flag for MSVC should only appear after \"export\"\n%!";
          cygwin := true;
          detected := NotTried; (* not an attempt ! *)
          iter flags
        | "auto" | "true" ->
          if not ( auto_detect_visual_studio !sixtyfourp ) then iter flags
        | "9.0" ->
          if not ( VS_9_0.detect_visual_studio !sixtyfourp ) then iter flags
        | "11.0" ->
          if not ( VS_11_0.detect_visual_studio !sixtyfourp ) then iter flags
        | "12.0" ->
          if not ( VS_12_0.detect_visual_studio !sixtyfourp ) then iter flags
        | "14.0" ->
          if not ( VS_14_0.detect_visual_studio !sixtyfourp ) then iter flags
        | "" -> iter flags
        | v ->
          Printf.eprintf "Error: OCPWIN_MSVC=%S not understood" v;
          exit 2
        end
    in
    try
      iter flags
    with exn ->
      Printf.eprintf "ocpwin-msvc: auto-detect failed with exn %S\n%!"
        (Printexc.to_string exn);
      exit 2

(* This is only called on Win32 when msvc is not selected. It will
   add $OCAMLLIB/mingw where lib*.a files should be stored. *)
let detect_mingw () =
  let msvc_env = read_MSVC_env "mingw" in
  let { env_LIB; env_LIBPATH } = msvc_env in
  let mingw_libs = Config.standard_library // "mingw" in
  add_to_path mingw_libs env_LIB;
  add_to_path mingw_libs env_LIBPATH;
  write_MSVC_env msvc_env;
  ()

let maybe_detect_env =
  let todo = ref true in
  function () ->
    if !todo then begin
      todo := false;
      if Config.os_type = "Win32" then
        if Config.ccomp_type = "msvc" then begin
          if !Clflags.detect_msvc <> "false" then
            detect_visual_studio  !Clflags.detect_msvc
        end else
          detect_mingw ()
    end
