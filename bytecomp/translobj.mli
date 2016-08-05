(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lambda

val oo_prim: string -> lambda

val share: structured_constant -> lambda
val meth: lambda -> string -> lambda * lambda list

val reset_labels: unit -> unit
val transl_label_init: Lambda.location -> lambda -> lambda
val transl_store_label_init: Lambda.location ->
    string -> Ident.t -> int -> ('a -> lambda) -> 'a -> int * lambda

val method_ids: IdentSet.t ref (* reset when starting a new wrapper *)

val oo_wrap: Lambda.location -> Env.t -> bool -> ('a -> lambda) -> 'a -> lambda
val oo_add_class: Ident.t -> Lambda.locid -> Env.t * bool

val reset: unit -> unit
