(* TEST
   flags = "-g"
   * native
   exit_status = "2"
 *)

let rec f x =
  if !x = 0 then raise Not_found;
  1+ f (ref (!x-1))

let main () =
  Printexc.record_backtrace true;
  f (ref 2000)

let _ = main ()
