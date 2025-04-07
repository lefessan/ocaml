(* TEST
   flags = "-g"
   * native
   exit_status = "2"
 *)

let rec f x =
  if !x = 0 then raise Not_found;
  1 + g (!x-1)

and g x =
  1 + f (ref x)

let main () =
  Printexc.record_backtrace true;
  f (ref 1000)

let _ = main ()
