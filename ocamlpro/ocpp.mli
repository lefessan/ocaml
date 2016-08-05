(***********************************************************************)
(*                                                                     *)
(*                              TypeRex                                *)
(*                                                                     *)
(*  Copyright 2014, Fabrice Le Fessant, OCamlPro.                      *)
(*                    All rights reserved.                             *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

val register : unit -> unit

val set_ocaml_version : string -> unit
val add_macro : string -> Parser.token list -> unit

val name_of_token : Parser.token -> string
val string_of_token : Parser.token -> string

val last_error : Location.t option ref
val lexbuf : unit -> Lexing.lexbuf

val add_option : string -> (string -> string -> unit) -> unit

(* patch: internal hooks for parsetrees *)
(* by convention, rewriters should start with a two digit number "NN_name".
   90-99 are used for compilation rewriters. 80-89 for analysis rewriters.
   Code generation rewriters should be in 10-49.   *)
val add_internal_interface_rewriter : string ->
  (string -> Parsetree.signature -> Parsetree.signature) -> unit
val add_internal_implementation_rewriter : string ->
  (string -> Parsetree.structure -> Parsetree.structure) -> unit
val apply_internal_interface_rewriters :
  string -> Parsetree.signature -> Parsetree.signature
val apply_internal_implementation_rewriters :
  string -> Parsetree.structure -> Parsetree.structure
