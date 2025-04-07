
let f5 f stack_size =
  Printf.eprintf "f5\n%!";
  f (ref stack_size)

let check_backtrace level exn =
    let s = Printexc.get_backtrace () in
    let bt = Printexc.get_raw_backtrace () in
    Printf.eprintf
      "%s: exception with backtrace (Printexc.get_backtrace):\n%s\n%!" level s;
    Printf.eprintf
      "%s: exception with backtrace (Printexc.raw_backtrace_to_string):\n%s\n%!" level
      (Printexc.raw_backtrace_to_string bt);
    Printf.eprintf "raw_backtrace_length: %d\n%!"
      (Printexc.raw_backtrace_length bt);
    let bt_entries : Printexc.raw_backtrace_entry array = Printexc.raw_backtrace_entries bt in
    let bt_slots : Printexc.backtrace_slot array option = Printexc.backtrace_slots bt in
    match bt_slots with
    | None -> assert false
    | Some bt_slots ->
    Array.iteri (fun i e ->
      let bsao : Printexc.backtrace_slot array option = Printexc.backtrace_slots_of_raw_entry e in
      match bsao with
      | None ->
         Printf.eprintf "bt_entries[%d] = none\n%!" i
      | Some bsa ->
         Array.iteri (fun n bs ->
           let  s =
           match Printexc.Slot.format 1 bs with
           | None -> "<none>"
           | Some s -> s in
           Printf.eprintf "bt_entries[%d.%d] = '%s'\n%!" i n s
         ) bsa
    ) bt_entries ;
    Printexc.raise_with_backtrace exn bt

let f4 f stack_size =
  Printf.eprintf "f4\n%!";
  try
    f5 f stack_size
  with
  | exn ->
    check_backtrace "f4" exn

let f3 f stack_size =
  Printf.eprintf "f3\n%!";
  try
    f4 f stack_size
  with
  | exn ->
    check_backtrace "f3" exn

let f2 f stack_size =
  Printf.eprintf "f2\n%!";
  try
    f3 f stack_size
  with
  | exn ->
    check_backtrace "f2" exn

let f1 f stack_size =
  Printf.eprintf "f1\n%!";
  try
    f2 f stack_size
  with
  | exn ->
    check_backtrace "f1" exn

let main f stack_size =
  Printexc.record_backtrace true;
  f1 f stack_size
