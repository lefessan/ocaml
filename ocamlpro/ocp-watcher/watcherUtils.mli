(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2015 OCamlPro                                                 *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

val send_request :
  WatcherCompilerProtocol.compiler_request -> string list -> unit

val set_imports_function : (unit -> (string * Digest.t option) list) -> unit

val env_imports : string list -> (string * string option) list

val from_suff : string -> string list -> (string * string option) list

val to_absolute : string -> (string * string option)

val context : bool -> WatcherCompilerProtocol.context

val register_crc: string -> string -> unit

val if_comp_enabled1 : ('a -> unit) -> ('a -> unit) -> 'a -> unit
val if_comp_enabled2 :
  ('a -> 'b -> unit) -> ('a -> 'b -> unit) -> 'a -> 'b -> unit
val if_comp_enabled3 :
  ('a -> 'b -> 'c -> unit) ->
  ('a -> 'b -> 'c -> unit) -> 'a -> 'b -> 'c -> unit
val if_comp_enabled4 :
  ('a -> 'b -> 'c -> 'd -> unit) ->
  ('a -> 'b -> 'c -> 'd -> unit) -> 'a -> 'b -> 'c -> 'd -> unit
