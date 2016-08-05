(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The toplevel directives. *)

open Format

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_cd : string -> unit
val dir_load : formatter -> string -> unit
val dir_use : formatter -> string -> unit
val dir_install_printer : formatter -> Longident.t -> unit
val dir_remove_printer : formatter -> Longident.t -> unit

(* tryocaml: ad-hoc implementation of polymorphic printers *)
type 'a printer_type2 = Env.t -> Types.type_expr -> 'a -> Outcometree.out_value
type 'a printer_type1 = Format.formatter -> 'a -> unit
type 'a printer_type0 = 'a -> unit

(* For topmain.ml. Maybe shouldn't be there *)
val load_file : formatter -> string -> bool
