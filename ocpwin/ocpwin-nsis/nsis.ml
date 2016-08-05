

type file_kind = File | Directory

let save_files_to_uninstall filename files =
  let oc = open_out filename in
  let dirs = ref [] in
  List.iter (fun (kind, file) ->
    match kind with
    | Directory -> dirs := file :: !dirs
    | File -> Printf.fprintf oc "%s \"%s\"\n%!" "Delete" file
  ) files;
  let dirs = List.rev (List.sort compare !dirs) in
  List.iter (fun dir ->
    Printf.fprintf oc "%s \"%s\"\n%!" "RMDir" dir
  ) dirs;
  close_out oc
