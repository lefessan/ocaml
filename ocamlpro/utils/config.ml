


let path_sep = if Sys.win32 then ';' else ':'

let env_path =
  try
    String.split_on_char path_sep (Sys.getenv "PATH")
  with Not_found -> []

(* When called from the Windows shell, Sys.executable_name is not
  correct, we have to search it again in the PATH to make it
  absolute. *)
let executable_name =
  let executable_name = Sys.executable_name in
  if Filename.is_implicit executable_name then
    let executable_name =
      if Sys.win32 && not ( Filename.check_suffix executable_name ".exe" ) then
        executable_name ^ ".exe" else
        executable_name in
    try
      Ocpstd.find_in_path env_path executable_name
    with Not_found ->
      executable_name
  else executable_name

let (standard_library_default, standard_runtime) =
  let default = standard_library in
  if Filename.is_relative default then
    let bindir = Filename.dirname executable_name in
    (* -relocatable: find OCAMLLIB relative to "ocamlrun" executable *)
    let ocamlrun_search_path = bindir :: env_path in
    let rec iter path =
      match path with
      | [] -> (default, standard_runtime)
      | bindir :: path ->
        let prefix = Filename.dirname bindir in
        let standard_runtime = Filename.concat bindir "ocamlrun" in
        if Sys.file_exists standard_runtime ||
          Sys.file_exists (standard_runtime ^ ".exe")
        then
          let standard_library_default = Filename.concat prefix default in
          (standard_library_default, standard_runtime)
        else
          iter path
    in
    iter ocamlrun_search_path
  else
    (default, standard_runtime)

let bindir = Filename.dirname standard_runtime

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default




let ocp_magic s =
  if want_ocp_magic then
    let len = String.length s in
    "OCP-" ^ (String.sub s 4 (len-4))
  else s

let cmo_magic_number = ocp_magic cmo_magic_number
let cma_magic_number = ocp_magic cma_magic_number
let cmx_magic_number = ocp_magic cmx_magic_number
let cmxa_magic_number = ocp_magic cmxa_magic_number
let cmxs_magic_number = ocp_magic cmxs_magic_number
let cmt_magic_number = ocp_magic cmt_magic_number
let exec_magic_number = ocp_magic exec_magic_number
let ast_impl_magic_number = ocp_magic ast_impl_magic_number
let ast_intf_magic_number = ocp_magic ast_intf_magic_number




let detect_msvc = "false"

let bindir = ref bindir
let supports_shared_libraries = ref supports_shared_libraries
let mksharedlib = ref mksharedlib
let byteccrpath = ref byteccrpath
let nativeccrpath = ref nativeccrpath
let mksharedlibrpath = ref mksharedlibrpath
let mklib1 = ref mklib1
let mklib2 = ref mklib2

let mkdll = ref mkdll
let mkexe = ref mkexe
let mkmaindll = ref mkmaindll
let bytecomp_c_compiler = ref bytecomp_c_compiler
let ccomp_type = ref ccomp_type
let bytecomp_c_libraries = ref bytecomp_c_libraries
let native_c_compiler = ref native_c_compiler
let native_c_libraries = ref native_c_libraries
let native_pack_linker = ref native_pack_linker
let ranlib = ref ranlib
let cc_profile = ref cc_profile
let architecture = ref architecture
let model = ref model
let system = ref system
let asm = ref asm
let asm_cfi_supported = ref asm_cfi_supported
let with_frame_pointers = ref with_frame_pointers
let ext_cc_obj = ref ext_cc_obj
let ext_obj = ref ext_obj
let ext_asm = ref ext_asm
let ext_lib = ref ext_lib
let ext_dll = ref ext_dll
let ar = ref ar
let detect_msvc = ref detect_msvc


module StringMap = Map.Make(String)

let _ =
  let config_filename, should_exist =
    try Sys.getenv "OCPWIN_CONFIG" , true with Not_found ->
      try Sys.getenv "OCPCOMP_FILE", true with Not_found ->
        "ocaml.config", false
  in
  let config_filename =
    if Filename.is_implicit config_filename then
      Filename.concat standard_library_default config_filename
    else config_filename
  in
  if Sys.file_exists config_filename then begin
    let relocatable = ref false in
    let flexlink = ref true in
    let options =
      let set_bool b s =
        match s with
        | "true" -> b := true
        | "false" -> b := false
        | _ -> failwith "bad boolean"
      in
      let set_string b s = b := s in
      let set_strings b s =
        List.iter (fun b -> b := s) b in
      let options = ref StringMap.empty in
      List.iter (fun (name, set) ->
        options := StringMap.add name set !options)
        [
          (* ocamlmklib *)
          "bindir", set_string bindir;
          "supports_shared_libraries", set_bool supports_shared_libraries;
          "mksharedlib", set_string mksharedlib;
          "byteccrpath", set_string byteccrpath;
          "nativeccrpath", set_string nativeccrpath;
          "mksharedlibrpath", set_string mksharedlibrpath;
          "mklib1", set_string mklib1;
          "mklib2", set_string mklib2;

          (* added *)
          "relocatable", set_bool relocatable;
          "detect_msvc", set_string detect_msvc;
          "mkdll", set_string mkdll;
          "mkexe", set_string mkexe;
          "mkmaindll", set_string mkmaindll;
          "flexlink", set_bool flexlink;
          "ext_cc_obj", set_string ext_cc_obj;


          (* standard *)
          "ccomp_type", set_string ccomp_type;
          "bytecomp_c_compiler", set_string bytecomp_c_compiler;
          "bytecomp_c_libraries", set_string bytecomp_c_libraries;
          "native_c_compiler", set_string native_c_compiler;
          "native_c_libraries", set_string native_c_libraries;
          "native_pack_linker", set_string native_pack_linker;
          "ranlib", set_string ranlib;
          "cc_profile", set_string cc_profile;
          "architecture", set_string architecture;
          "model", set_string model;
          "system", set_string system;
          "asm", set_string asm;
          "asm_cfi_supported", set_bool asm_cfi_supported;
          "with_frame_pointers", set_bool with_frame_pointers;
          "ext_obj", set_strings [ext_obj; ext_cc_obj];
          "ext_asm", set_string ext_asm;
          "ext_lib", set_string ext_lib;
          "ext_dll", set_string ext_dll;
          "ar", set_string ar;
        ];
      !options
    in
    Ocpstd.iteri_lines (fun i line ->
      let len = String.length line in
      if len > 0 && line.[0] <> '#' then
        let (var, value) = Ocpstd.cut_at line ':' in
        try
          (StringMap.find var options) value
        with
        | e ->
          Printf.eprintf "Error %S at %s:%d\n%!" (Printexc.to_string e) config_filename i;
          exit 2
    ) config_filename;

    if !flexlink then begin
      (* We must add and quote flexlink... *)
      let flexlink =
        if !relocatable then
          Filename.quote (Filename.concat !bindir "flexlink")
        else "flexlink" in
      mkdll := flexlink ^ !mkdll;
      mkexe := flexlink  ^ !mkexe;
      mkmaindll := flexlink ^ !mkmaindll;
    end
    else
      if should_exist then
        Printf.eprintf "Error: missing ocaml compiler configuration file %S\n%!"
          config_filename


  end


let _ = (* provides a sensible default value *)
  if !mklib1 = "" then begin
    mklib1 := "ar rc ";
    mklib2 := "ranlib"
  end

let bindir = !bindir
let supports_shared_libraries = !supports_shared_libraries
let mksharedlib = !mksharedlib
let byteccrpath = !byteccrpath
let nativeccrpath = !nativeccrpath
let mksharedlibrpath = !mksharedlibrpath
let mklib1 = !mklib1
let mklib2 = !mklib2

let mkdll = !mkdll
let mkexe = !mkexe
let mkmaindll = !mkmaindll
let bytecomp_c_compiler = !bytecomp_c_compiler
let ccomp_type = !ccomp_type
let bytecomp_c_libraries = !bytecomp_c_libraries
let native_c_compiler = !native_c_compiler
let native_c_libraries = !native_c_libraries
let native_pack_linker = !native_pack_linker
let ranlib = !ranlib
let cc_profile = !cc_profile
let architecture = !architecture
let model = !model
let system = !system
let asm = !asm
let asm_cfi_supported = !asm_cfi_supported
let with_frame_pointers = !with_frame_pointers
let ext_cc_obj = !ext_cc_obj
let ext_obj = !ext_obj
let ext_asm = !ext_asm
let ext_lib = !ext_lib
let ext_dll = !ext_dll
let ar = !ar

(* Sys.os_type is the system on which "ocamlopt" is running, while
   Config.os_type is the system for which the code generated by
   ocamlopt is targetted. *)
let os_type = match system with
  | "mingw"
  | "mingw32"
  | "mingw64"
  | "win32"
  | "win64" -> "Win32"
  | "cygwin" -> "Cygwin"
  | _ -> "Unix"


(* Disable warning for unused default_executable_name *)
let _ = assert (default_executable_name <> "")
let default_executable_name =
  match os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"




let print_extended_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  let p_bool name valu = Printf.fprintf oc "%s: %B\n" name valu in

  p "target_os_type" os_type;
  p "detect_msvc" !detect_msvc;

  p "mkdll" mkdll;
  p "mkexe" mkexe;
  p "mkmaindll" mkmaindll;
  p "ext_cc_obj" ext_cc_obj;

  p "bindir" bindir;
  p_bool "supports_shared_libraries" supports_shared_libraries;
  p "mksharedlib" mksharedlib;
  p "byteccrpath" byteccrpath;
  p "nativeccrpath" nativeccrpath;
  p "mksharedlibrpath" mksharedlibrpath;
  p "mklib1" mklib1;
  p "mklib2" mklib2;

  flush oc;
;;
