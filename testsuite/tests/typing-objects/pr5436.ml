(* Marshaling (cf. PR#5436) *)

let r = ref 0;;
let id o = Oo.id o - !r;;
r := Oo.id (object end);;
id (object end);;
id (object end);;

Printf.printf "(1)\n%!";;

let o = object end in
  let s = Marshal.to_string o [] in
  let o' : < > = Marshal.from_string s 0 in
  let o'' : < > = Marshal.from_string s 0 in
  (id o, id o', id o'');;

Printf.printf "(2)\n%!";;

let o = object val x = 33 method m = x end in
  let s = Marshal.to_string o [Marshal.Closures] in
  let o' : <m:int> = Marshal.from_string s 0 in
  let o'' : <m:int> = Marshal.from_string s 0 in
  (id o, id o', id o'', o#m, o'#m);;

Printf.printf "(3)\n%!";;

let o = object val x = 33 val y = 44 method m = x end in
  let s = Marshal.to_string (o,o) [Marshal.Closures] in
  let (o1, o2) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  let (o3, o4) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  (id o, id o1, id o2, id o3, id o4, o#m, o1#m);;
Printf.printf "(4)\n%!";;
