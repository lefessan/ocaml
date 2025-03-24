(* TEST
   flags = "-g"
   ocamlrunparam += ",b=5"
   * native
   exit_status = "2"
 *)

let rec f x =
  if !x = 0 then raise Not_found;
  1+ f (ref (!x-1))

let f5 () =
  Printf.eprintf "f5\n%!";
  f (ref 1000)

let f4 () =
  Printf.eprintf "f4\n%!";
  try
    f5 ()
  with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printf.eprintf "f4: exception with backtrace:\n%s\n%!"
      (Printexc.raw_backtrace_to_string bt);
    Printexc.raise_with_backtrace exn bt

let f3 () =
  Printf.eprintf "f3\n%!";
  try
    f4 ()
  with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printf.eprintf "f3: exception with backtrace:\n%s\n%!"
      (Printexc.raw_backtrace_to_string bt);
    Printexc.raise_with_backtrace exn bt

let f2 () =
  Printf.eprintf "f2\n%!";
  try
    f3 ()
  with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printf.eprintf "f2: exception with backtrace:\n%s\n%!"
      (Printexc.raw_backtrace_to_string bt);
    Printexc.raise_with_backtrace exn bt

let f1 () =
  Printf.eprintf "f1\n%!";
  try
    f2 ()
  with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printf.eprintf "f1: exception with backtrace:\n%s\n%!"
      (Printexc.raw_backtrace_to_string bt);
    Printexc.raise_with_backtrace exn bt


let main () =
  f1 ()

let _ =
  Printexc.record_backtrace true;
  main ()
