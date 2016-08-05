(* These functions implement a cache of source files, where source
   files are indexed both by their MD5 digests (in ~/.ocp/sources/) and
   the MD5 digests of the artefacts (.cmi, .cmx) they produce.

   Caching can be disabled by setting the environment variable
    OCP_CACHE_DISABLED .

   Author: Fabrice Le Fessant (INRIA/OCamlPro)
*)

val register_ml : string -> unit
val register_mli : string -> unit
val register_pack_cmi : Digest.t -> unit
val register_pack_cmx : Digest.t -> unit
val clear_sources : unit -> unit

val register_cmi : Digest.t -> unit
val register_cmx : Digest.t -> unit
