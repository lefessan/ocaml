(* TEST
   flags = "-g"
   * native
   exit_status = "2"
 *)

(* This test checks whether we can detect a cycle containing
   inline calls and still have it correctly displayed. *)

let apply1 f x = (* inlined *)
  f x * 2

let apply2 f x = (* inlined *)
  apply1 f x + 9

let rec f x =
  if !x = 0 then raise Not_found;
  1 + g !x

and g x =
  1 + h (x-1)

and h x =
  1 + apply2 f (ref x)

let main () =
  Printexc.record_backtrace true;
  f (ref 700)

let _ = main ()
