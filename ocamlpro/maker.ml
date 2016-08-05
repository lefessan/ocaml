(***********************************************************************)
(*                                                                     *)
(*                              TypeRex                                *)
(*                                                                     *)
(*  Copyright 2014, Fabrice Le Fessant, INRIA/OCamlPro.                *)
(*                    All rights reserved.                             *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

(*
  Use the "-make SOURCE.ml" option to specify which file is the main of the
  application. You can use "-o TARGET" before to specify the name of the
  executable to build, otherwise, it will be "SOURCE". The compiler will
  generate a file "SOURCE.cmxd" or "SOURCE.cmod" containing the digests of
  the sources/targets. It is used to avoid rebuilding everything everytime.
  If you delete it, everything will be rebuilt the next time.

  Use "-I DIR" to specify where the sources are. "DIR" must be
  relative directories for sources. Absolute directories are
  considered as containing already built targets, and nothing will be
  built inside.  If a directory inside your project contains things
  that should not be built, you should use "-I `pwd`/DIR" to make the
  directory absolute.

  Lexers (.mll) and parsers (.mly) are built using "ocamllex" and "ocamlyacc".
  You can specify these commands by setting the OCAMLLEX and OCAMLYACC
  environment commands.

  Env variables:
   OCP_DEBUGMAKER: trigger debugging messages
   OCAMLLEX : command to run ocamllex on .mll files
   OCAMLYACC : command to run ocamlyacc on .mly files

  Implementation details:
  * the main asks for the compilation of "SOURCE.ml". It also sets internal
  parsetree rewriters to intercept the AST, compute dependencies on it,
  and ask for the compilation of these dependencies. Their AST are loaded,
  and it recursively explores the tree of dependencies, compiling them only
  when needed. If a file should not be compiled, an exception
   "Misc.RewriterExit (NoCompilationNeeded digest)" is raised to exit the
  compilation process without triggering an error.
  At the end, a topological sort (the Toposort module) is done on object files
  to link them.
  The ".cmod" and ".cmxd" files contain a marshaled StringMap of digests.
  For each source, the digest is a combination of the source digest, the
  target digests and a digest of all the dependencies digests.
*)

module StringMap = Map.Make(String)

type kind =
  | PRESENT
  | GENERATED of string

type file = {
  name : string;
  node : Toposort.node;
  mutable deps : file list;
}

let debug = try ignore (Sys.getenv "OCP_DEBUGMAKER"); true with _ -> false

exception NoCompilationNeeded of string
let n_intfs = ref 0
let n_impls = ref 0
let filename_stack = ref []
let compiled = ref StringMap.empty

(* [digest_file] is the target with ".cmxd" in native, and ".cmod" in
 bytecode *)
let digest_file = ref ""

let digests = ref StringMap.empty
let old_digest = Digest.string "x"
let file_digest = Digest.string "y"

let load_digests filename =
  try
    digest_file := filename;
    let ic = open_in_bin !digest_file in
    digests := input_value ic;
    close_in ic
  with _ -> ()
let save_digests () =
  let oc = open_out_bin !digest_file in
  output_value oc !digests;
  close_out oc

let old_digest src =
  try
    StringMap.find src !digests
  with Not_found -> old_digest
let file_digest src =
  try Digest.file src with _ -> file_digest

let last_save = ref 0
let maybe_save () =
  if !n_impls + !n_intfs <> !last_save then begin
    save_digests ();
    last_save := !n_impls + !n_intfs
  end

let maker_path = ref []

let add_dir dir =
  if dir = "--" then begin
    maker_path := [];
    false
  end else begin
    maker_path := !maker_path @ [ dir ];
    true
  end

let make_filemaps () =
  let path = "" :: !maker_path in
  let intf = ref StringMap.empty in
  let impl = ref StringMap.empty in

  let rec iter = function
      [] -> ()
    | dir :: path ->
      iter path;
      if Filename.is_relative dir ||
         Filename.is_implicit dir then
        try
(* Interesting bug: under Windows, Sys.readdir on a non-existing dir
  returns [||] without any error... *)
          let files = Sys.readdir (if dir = "" then "." else dir) in
          Array.iter (fun file ->
            if Filename.check_suffix file ".ml" then begin
              let modname =
                String.capitalize (Filename.chop_suffix file ".ml") in
              let fullname = Filename.concat dir file in
              impl := StringMap.add modname (fullname, PRESENT) !impl;
              if not ( Sys.file_exists (fullname ^ "i") ) then
                intf := StringMap.add modname (fullname, PRESENT) !intf
            end else
            if Filename.check_suffix file ".mli" then
              let modname =
                String.capitalize (Filename.chop_suffix file ".mli") in
              let fullname = Filename.concat dir file in
              intf := StringMap.add modname (fullname, PRESENT) !intf
          ) files;
          Array.iter (fun file ->
            if Filename.check_suffix file ".mll" then begin
              let basename = Filename.chop_suffix file ".mll" in
              let modname = String.capitalize basename in
              let fullbase = Filename.concat dir basename in
              impl := StringMap.add modname
                  (fullbase^".ml", GENERATED (fullbase ^ ".mll"))
                  !impl;
              if not ( Sys.file_exists (fullbase ^ ".mli") ) then
                intf := StringMap.add modname
                     (fullbase^".ml", GENERATED (fullbase ^ ".mll"))
                     !intf
            end else
            if Filename.check_suffix file ".mly" then
              let basename = Filename.chop_suffix file ".mll" in
              let modname = String.capitalize basename in
              let fullbase = Filename.concat dir basename in
              intf := StringMap.add modname
                   (fullbase^".mli", GENERATED (fullbase ^ ".mly"))
                   !intf;
              impl := StringMap.add modname
                   (fullbase^".ml", GENERATED (fullbase ^ ".mly"))
                   !impl;
          ) files;

        with _ -> ()
  in
  iter path;
  (!intf, !impl)

let maybe_compile compile src =
  if debug then Printf.eprintf "    maybe_compile %S\n%!" src;
  try
    let digest = StringMap.find src !compiled in
    match !digest with
    | None ->
      Printf.eprintf "recursive dependencies on %S\n%!" src;
      exit 2
    | Some d -> d
  with Not_found ->
    if debug then Printf.eprintf "    maybe_compile %S\n%!" src;
    let old_filename_stack = !filename_stack in
    let digest = ref None in
    compiled := StringMap.add src digest !compiled;
    let d = try
        let make_digest = ref (fun () -> assert false) in
        filename_stack := (src, make_digest) :: !filename_stack;
        compile src;
        let d = !make_digest () in
        digests := StringMap.add src d !digests;
        save_digests ();
        d
      with NoCompilationNeeded d -> d
    in
    digest := Some d;
    filename_stack := old_filename_stack;
    d

let source () =
  match !filename_stack with
    [] -> assert false
  | file :: _ -> file

let modulename file =
  let file = String.capitalize (Filename.basename file) in
  if Filename.check_suffix file ".ml" then
    Filename.chop_suffix file ".ml"
  else
  if Filename.check_suffix file ".mli" then
    Filename.chop_suffix file ".mli"
  else assert false

let target file suffix =
    if Filename.check_suffix file ".ml" then
      (Filename.chop_suffix file ".ml") ^ suffix
    else
    if Filename.check_suffix file ".mli" then
      (Filename.chop_suffix file ".mli") ^ suffix
    else assert false

let obj_files = ref []
let files = ref StringMap.empty
let new_file name =
  try
    StringMap.find name !files
  with Not_found ->
    let file = { name ; node = Toposort.new_node (); deps = [] }in
    files := StringMap.add name file !files;
    file

module Sort = Toposort.Make(struct
    type t = file
    let node file = file.node
    let iter_deps f file = List.iter f file.deps
  end)

let ocamllex = try Sys.getenv "OCAMLLEX" with Not_found -> "ocamllex"
let ocamlyacc = try Sys.getenv "OCAMLYACC" with Not_found -> "ocamlyacc"

let sys_command cmd =
  Printf.eprintf "\n%s:\n%!" cmd;
  Ocpstd.Sys.system cmd

let check_generated = function
  | PRESENT -> ()
  | GENERATED src ->
    let d = Digest.file src in
    let old_digest = old_digest src in
    if Filename.check_suffix src ".mll" then begin
      let ml = Filename.chop_suffix src "l" in
      if d <> old_digest || not (Sys.file_exists ml) then begin
        let retcode = Printf.kprintf sys_command "%s %s"
            ocamllex (Filename.quote src) in
        if retcode <> 0 then exit 2;
        digests := StringMap.add src d !digests;
        save_digests ()
      end
    end else begin (* mly *)
      let ml = Filename.chop_suffix src "y" in
      let mli = ml ^ "i" in
      if d <> old_digest
      || not (Sys.file_exists ml)
      || not (Sys.file_exists mli) then begin
        let retcode = Printf.kprintf sys_command "%s -v %s"
            ocamlyacc (Filename.quote src) in
        if retcode <> 0 then exit 2;
        digests := StringMap.add src d !digests;
        save_digests ()
      end

    end

let check_interface (intfs, impls) compile _sourcefile intf =
  let src, make_digest = source () in
  let modname = modulename src in

  Depend.free_structure_names := Depend.StringSet.empty;
  if modname <> "Pervasives" && not !Clflags.nopervasives then
    Depend.free_structure_names := Depend.StringSet.add "Pervasives"
        !Depend.free_structure_names;
  Depend.add_signature Depend.StringSet.empty intf;
  let deps = ! Depend.free_structure_names in
  let intfs_before = !n_intfs in

  let dbuf = Buffer.create 100 in
  let cmi = target src ".cmi" in
  let file = new_file src in
  if debug then Printf.eprintf "Checking interface deps for %S\n%!" src;
  Depend.StringSet.iter (fun modname ->
    if debug then Printf.eprintf "      %S needed by %S\n%!" modname src;
    try
      let (dep, kind) = StringMap.find modname intfs in
      check_generated kind;
      file.deps <- new_file dep :: file.deps;
      Buffer.add_string dbuf (maybe_compile compile dep);
    with Not_found -> ()
  ) deps;
  if debug then Printf.eprintf "   Interface deps for %S done\n%!" src;
  let dbuf = Digest.string (Buffer.contents dbuf) in
  let d = (Digest.file src) ^ (file_digest cmi) ^ dbuf in

  let old_digest = old_digest src in
  if Sys.file_exists cmi &&
     !n_intfs = intfs_before &&
     d = old_digest then begin
    if debug then Printf.eprintf "        Skipping %S\n" src;
    raise (Ocputils.RewriterExit (NoCompilationNeeded d));
  end;

  make_digest := (fun () ->
    (Digest.file src) ^ (Digest.file cmi) ^ dbuf
  );

  incr n_intfs;
  if debug then Printf.eprintf "Compile interface %S\n%!" src;

  Location.input_name := src;
  Compmisc.init_path false;
  Env.set_unit_name (modulename src);

  Printf.eprintf " %s.cmi%!" (modulename src);
  intf

let rec check_implementation nativep (intfs, impls) compile  _sourcefile impl =
  let src, make_digest = source () in
  let modname = modulename src in

  Depend.free_structure_names := Depend.StringSet.empty;
  if modname <> "Pervasives" && not !Clflags.nopervasives then
    Depend.free_structure_names := Depend.StringSet.add "Pervasives"
        !Depend.free_structure_names;
  Depend.add_implementation Depend.StringSet.empty impl;
  let deps = ! Depend.free_structure_names in

  let intfs_before = !n_intfs in
  let impls_before = !n_impls in
  let dbuf = Buffer.create 100 in
  let cmi = target src ".cmi" in
  let cmo = target src (if nativep then ".cmx" else ".cmo") in
  let file = new_file src in
  if debug then Printf.eprintf "Checking implementation deps for %S\n%!" src;
  Depend.StringSet.iter (fun modname ->
    if debug then Printf.eprintf "      %S needed by %S\n%!" modname src;
    try
      file.deps <-
        ( check_module dbuf nativep (intfs, impls) compile modname ) @
        file.deps
    with Not_found -> ()
  ) deps;
  let dbuf = Digest.string (Buffer.contents dbuf) in
  if debug then Printf.eprintf "   Implementation deps for %S done\n%!" src;
  obj_files := file :: !obj_files;
  let d = (Digest.file src) ^ (file_digest cmi) ^ (file_digest cmo) ^ dbuf in
  let old_digest = old_digest src in
  if !n_intfs = intfs_before &&
     (!n_impls = impls_before || not nativep) &&
     d = old_digest &&
     Sys.file_exists cmi &&
     Sys.file_exists cmo then begin
    if debug then Printf.eprintf "        Skipping %S\n" src;
    raise (Ocputils.RewriterExit (NoCompilationNeeded d));
  end;

  make_digest := (fun () ->
    (Digest.file src) ^ (Digest.file cmi) ^ (Digest.file cmo) ^ dbuf
  );
  incr n_impls;
  if not (Sys.file_exists (src ^ "i")) then incr n_intfs;
  if debug then Printf.eprintf "Compile implementation %S\n%!" src;

  Location.input_name := src;
  Compmisc.init_path nativep;
  Env.set_unit_name (modulename src);
  Printf.eprintf " %s%s%!" (modulename src) (if nativep then ".cmx" else ".cmo");
  impl

and check_module dbuf nativep (intfs, impls) compile modname =
  let (dep, kind) = StringMap.find modname intfs in
  check_generated kind;
  let intf_dep = new_file dep in
  Buffer.add_string dbuf (maybe_compile compile dep);
  try
    let (dep, kind) = StringMap.find modname impls in
    check_generated kind;
    let impl_dep = new_file dep in
    Buffer.add_string dbuf (maybe_compile compile dep);
    [intf_dep; impl_dep]
  with Not_found ->
    [intf_dep]

let is_unit_name name =
  try
    Compenv.is_unit_name name
  with _ -> false

let modules_of_file filename =
  let modules = ref [] in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      if line <> "" then begin
        if not (is_unit_name line) then begin
          Printf.eprintf "Error in %S: %S is not a module name\n%!"
            filename line;
          exit 2
        end;
        modules := line :: !modules
      end
    done;
    assert false
  with End_of_file ->
    close_in ic;
    List.rev !modules

let make ppf nativep compile pack_link lib_link exe_link report_error filename =
  let modules, link =
    if Filename.check_suffix filename ".ml" then
      let basename = Filename.basename filename in
      let basename = Filename.chop_suffix basename ".ml" in
      let modname = String.capitalize basename in
      let dirname = Filename.dirname filename in
      Clflags.include_dirs := !Clflags.include_dirs @ [ dirname ];
      [modname], exe_link
    else
      if Filename.check_suffix filename ".mllib" then
        let modules = modules_of_file filename in
        let dirname = Filename.dirname filename in
        Clflags.include_dirs := !Clflags.include_dirs @ [ dirname ];
        begin match !Clflags.output_name with
          None ->
            let target_base = Filename.chop_suffix filename ".mllib" in
            let target_ext = if nativep then ".cmxa" else ".cma" in
            Clflags.output_name := Some (target_base ^ target_ext)
        | Some _ -> ()
        end;
        modules, lib_link
      else
      if Filename.check_suffix filename ".mlpack" then
        let target_base = Filename.chop_suffix filename ".mlpack" in
        begin match !Clflags.output_name with
          None ->
            let target_ext = if nativep then ".cmx" else ".cmo" in
            Clflags.output_name := Some (target_base ^ target_ext)
        | Some _ -> ()
        end;
        let basename = Filename.basename target_base in
        let modname = String.capitalize basename in
        Clflags.for_package := Some modname;
        let modules = modules_of_file filename in
        let dirname = Filename.dirname filename in
        Clflags.include_dirs := !Clflags.include_dirs @ [ dirname ];
        modules, pack_link
      else
        if is_unit_name filename then
          [filename], exe_link
        else begin
          Printf.eprintf "Argument %S of -make must be a module name/.ml/.mllib\n%!" filename;
          exit 2
        end
  in
  let basename = Filename.chop_suffix filename ".ml" in
  let compile name =
    try
      Compenv.readenv ppf Compenv.Before_compile;
      compile ppf name
    with e ->
      report_error ppf e;
      exit 2
  in
  try
    (* for objects and libraries provided before -make *)
    let objfiles = Compenv.get_objfiles () in
    Compmisc.init_path nativep;
    let target = match !Clflags.output_name with
      | None -> basename
      | Some filename -> filename in
    load_digests (target ^ (if nativep then ".cmxd" else ".cmod"));
    let filemaps = make_filemaps () in
    Ocpp.add_internal_interface_rewriter "99_intf_maker"
      (check_interface filemaps compile);
    Ocpp.add_internal_implementation_rewriter "99_impl_maker"
      (check_implementation nativep filemaps compile);
    let dbuf = Buffer.create 100 in
    List.iter (fun modname ->
      try
        let _deps = check_module dbuf nativep filemaps compile modname in
        ()
      with Not_found ->
        Printf.eprintf "Module %S not found\n%!" modname;
        exit 2
    ) modules;
(*
    let src = basename ^ ".ml" in
    let mli = src ^ "i" in
    if Sys.file_exists mli then
      ignore (maybe_compile compile mli);
    ignore (maybe_compile compile src);
*)
    if debug then Printf.eprintf "Linking...\n%!";
    let all_files = Sort.sort !obj_files in
    let ml_files = ref [] in
    List.iter (fun file ->
      let file = file.name in
      if Filename.check_suffix file ".ml" then
        ml_files := Filename.chop_suffix file ".ml" :: !ml_files) all_files;
    Compenv.readenv ppf Compenv.Before_link;
    let cm_files =  List.map (fun file ->
        let file = file ^ (if nativep then ".cmx" else ".cmo") in
        if debug then Printf.eprintf " %S" file;
        file
      ) (List.rev !ml_files) in
    if debug then Printf.eprintf "\n%!";
    Clflags.for_package := None;
    link ppf (objfiles @ cm_files) target;
    Printf.eprintf "\n= %s\n%!" target;
    save_digests ();
    Warnings.check_fatal ();
    exit 0
  with x ->
    report_error ppf x;
    exit 2

let register
    ppf nativep compile pack_link lib_link exe_link report_error =
  let make = make ppf nativep compile
    pack_link lib_link exe_link report_error in
  Ocpstd.Arg.default_arglist := (Main_args.mk_make make) :: !Ocpstd.Arg.default_arglist
