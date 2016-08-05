(***********************************************************************)
(*                                                                     *)
(*                              TypeRex                                *)
(*                                                                     *)
(*  Copyright 2014, Fabrice Le Fessant, INRIA/OCamlPro.                *)
(*                    All rights reserved.                             *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

val register :
  Format.formatter ->
  bool -> (* native ? *)
  (Format.formatter -> string -> unit) -> (* compile *)
  (Format.formatter -> string list -> string -> unit) -> (* pack_link *)
  (Format.formatter -> string list -> string -> unit) -> (* lib_link *)
  (Format.formatter -> string list -> string -> unit) -> (* exe_link *)
  (Format.formatter -> exn -> unit) -> (* report error *)
  unit

val add_dir : string -> bool
