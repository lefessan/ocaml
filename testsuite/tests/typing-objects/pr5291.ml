
(* Recursion (cf. PR#5291) *)

class a = let _ = new b in object end
and b = let _ = new a in object end;;

class a = let _ = new a in object end;;
