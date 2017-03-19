(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*     Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt       *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external myldexp : (float) -> (int) -> (float) =
  "caml_ldexp_float"

let f () =
  let p = 6061966420939680.
  and pexp = 22
  and q = 8711087657624833.
  and qexp = 83 in
  let a = myldexp ( p) pexp in
  Printf.printf "%f\n" a;
  let b = myldexp ( q) qexp in
  Printf.printf "%f\n" b;
  let c = a /. b in
  print_float c;
  print_newline ();
  Printf.printf "%.40f\n" c;
()

let () = f ()

