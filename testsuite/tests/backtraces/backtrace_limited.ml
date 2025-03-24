(* TEST
   flags = "-g"
   ocamlrunparam += ",b=3"
   * native
   exit_status = "2"
 *)

(* This test verifies that we can disable full backtraces
   using OCAMLRUNPARAM=b=3  even if Printexc.record_backtrace
   sets it to true. *)

let rec f x =
  if !x = 0 then raise Not_found;
  1 + g !x

and g x =
  1 + h (x-1)

and h x =
  1 + f (ref x)

let main () =
  Printexc.record_backtrace true;
  f (ref 700)

let _ = main ()
