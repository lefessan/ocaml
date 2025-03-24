(* TEST
   modules = "backtrace_main.ml"
   flags = "-g"
   * native
   exit_status = "2"
 *)

let rec f x =
  if !x = 0 then raise Not_found;
  1 + g !x

and g x =
  1 + h (x-1)

and h x =
  1 + f (ref x)

let _ =
  Backtrace_main.main f 700
