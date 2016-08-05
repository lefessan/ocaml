(* This implementation is the worst possible: since we don't have access
to basic functions, such as "mkdir" or "cp", we are forced to call
external shell commands, i.e. very bad portability and performance. *)

let home_dir = try Sys.getenv "HOME" with Not_found -> "/no-home-dir"
let save_in_cache_config = try
  ignore (Sys.getenv "OCP_SOURCES_ENABLED"); true
with Not_found -> false

let pwd = Sys.getcwd ()

let src_files = ref []
let clear_sources () = src_files := []

let register_ml filename =
  let filename = if Filename.is_implicit filename then
      Filename.concat pwd filename
    else filename in
  src_files := ("ml", filename, ref None) :: !src_files

let register_mli filename =
  let filename = if Filename.is_implicit filename then
      Filename.concat pwd filename
    else filename in
  src_files := ("mli", filename, ref None) :: !src_files

let register_pack_cmi digest =
  src_files := ("cmi", digest, ref (Some digest)) :: !src_files

let register_pack_cmx digest =
  src_files := ("cmx", digest, ref (Some digest)) :: !src_files

let create_hash_directory base_dir basename =
  let dirname0 = Filename.concat base_dir (String.make 1 basename.[0]) in
  let dirname1 = Filename.concat dirname0 (String.make 1 basename.[1]) in
  let dirname2 = Filename.concat dirname1 (String.make 1 basename.[2]) in
  let filename = Filename.concat dirname2 basename in

  if not (Sys.file_exists base_dir) then
    ignore (Printf.kprintf Ocpstd.Sys.system "mkdir %s" base_dir);

  if not (Sys.file_exists dirname0) then
    ignore (Printf.kprintf Ocpstd.Sys.system "mkdir %s" dirname0);

  if not (Sys.file_exists dirname1) then
    ignore (Printf.kprintf Ocpstd.Sys.system "mkdir %s" dirname1);

  if not (Sys.file_exists dirname2) then
    ignore (Printf.kprintf Ocpstd.Sys.system "mkdir %s" dirname2);

  filename

let save_in_cache cm_ext cm_digest =
  if save_in_cache_config && Sys.file_exists home_dir then
    try
      let ocp_dir = try
        Sys.getenv "OCP_DIR"
      with Not_found ->
        Filename.concat home_dir ".ocp"
      in

      let sources_dir = Filename.concat ocp_dir "sources" in
      let files_dir = Filename.concat ocp_dir "files" in

      if not (Sys.file_exists ocp_dir) then
        ignore (Printf.kprintf Ocpstd.Sys.system "mkdir %s" ocp_dir);

      let (src_ext, src_filename, src_digest) =
        match !src_files with
          [ ( "ml" | "mli" ) as src_ext, src_filename, src_digest ] ->
          let src_digest = match !src_digest with
            | Some digest -> digest
            | None ->
              let digest = Digest.file src_filename in
              let digest = Digest.to_hex digest in
              src_digest := Some digest;
              let digest_basename = digest ^ "." ^ src_ext in

              let digest_filename = create_hash_directory sources_dir
                  digest_basename in

              ignore (Printf.kprintf Ocpstd.Sys.system "cp %s %s"
                  src_filename
                  digest_filename
              );

              digest
          in
          (src_ext, src_filename, src_digest)
        | packed_files ->
          ("pack",
           String.concat ":" (List.map (fun (src_ext, digest, _) ->
               Printf.sprintf "%s.%s" (Digest.to_hex digest) src_ext)
               packed_files),
           "(pack)"
          )
      in
      let cm_basename = Printf.sprintf "%s.%s"
          (Digest.to_hex cm_digest) cm_ext in
      let cm_filename = create_hash_directory files_dir cm_basename in

      let oc = open_out cm_filename in
      Printf.fprintf oc "cm_crc=%s\n" (Digest.to_hex cm_digest);
      Printf.fprintf oc "src_crc=%s\n" src_digest;
      Printf.fprintf oc "cm_ext=%s\n" cm_ext;
      Printf.fprintf oc "src_ext=%s\n" src_ext;
      Printf.fprintf oc "src=%s\n" src_filename;
      Printf.fprintf oc "ocamllib=%s\n" Config.standard_library;
      Array.iteri (fun i arg ->
        Printf.fprintf oc "arg%d=%s\n" i arg
      ) Sys.argv;
      close_out oc;

    with exn ->
      Printf.eprintf "Warning: save_in_cache %s.%s failed\n"
        (Digest.to_hex cm_digest) cm_ext;
      Printf.eprintf "   with exception %S\n%!" (Printexc.to_string exn)

let register_cmi digest =
  save_in_cache "cmi" digest

let register_cmx digest =
  save_in_cache "cmx" digest

let register_cmo digest =
  save_in_cache "cmx" digest
