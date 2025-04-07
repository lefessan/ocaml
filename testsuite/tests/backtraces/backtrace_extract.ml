(* TEST
   modules = "backtrace_main.ml"
   flags = "-g"
   * native
   exit_status = "2"
 *)

let rec f x =
  if !x = 0 then raise Not_found;
  1+ f (ref (!x-1))

let _ =
  Backtrace_main.main f 2000
