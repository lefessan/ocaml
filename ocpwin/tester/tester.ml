let prefix_dir = Sys.argv.(1)

let commands =
  let lines = ref [] in
  let ic = open_in "commands.cmds" in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    assert false
  with End_of_file ->
    close_in ic;
    List.rev !lines

let sys_command cmd =
  if Sys.command cmd <> 0 then raise Exit

let _ =

  let workqueue = ref [] in
  let rec iter dir =
    let files = Sys.readdir dir in
    Array.iter (fun file ->
      let filename = Filename.concat dir file in
      if Sys.is_directory filename then
        iter filename
      else
          workqueue := filename :: !workqueue
    ) files
  in
  iter prefix_dir;

  let test_oc = open_out "files.test" in
  let yes_oc = open_out "files.yes" in
  let no_oc = open_out "files.no" in
  begin
    let oc = open_out "files.cmds" in
    List.iter (fun line ->
      Printf.fprintf oc "%s\n" line) commands;
    close_out oc
  end;

  let first_n n list =
    let rec iter n list tail =
      if n = 0 then List.rev tail, list else
        match list with
          [] -> List.rev tail, []
        | x :: list -> iter (n-1) list (x :: tail)
    in
    iter n list []
  in

  List.iter (fun n ->

    Printf.eprintf "Testing %d files by blocks of size %d\n%!"
      (List.length !workqueue) n;
    let rec iter list =
      match list with
        [] -> ()
      | _ ->
        let files, list = first_n n list in

        List.iteri (fun i filename ->
          let new_name = Filename.concat prefix_dir
            (Printf.sprintf "saved%d" i) in
          Printf.eprintf "mv %S %S\n%!" filename new_name;
          Sys.rename filename new_name) files;

        begin try
                List.iter sys_command commands;
                List.iter (fun filename ->
                  Printf.fprintf yes_oc "%s\n%!" filename) files;
          with _ ->
            workqueue := files @ !workqueue
        end;

        List.iteri (fun i filename ->
          let new_name = Filename.concat prefix_dir
            (Printf.sprintf "saved%d" i) in
          Sys.rename new_name filename) files;

        iter list
    in
    let list = !workqueue in
    workqueue := [];
    iter list

  ) [50; 10; 3; 1];

  List.iter (fun filename ->
    Printf.fprintf no_oc "%s\n%!" filename) !workqueue;

  close_out yes_oc;
  close_out no_oc;
  close_out test_oc;
