
(* patch: internal hooks for parsetrees *)
exception RewriterExit of exn
val fold_rewriters : (string * (string -> 'b -> 'b)) list -> string -> 'b -> 'b

module Clflags: sig
  val include_location_table : bool ref
  val dump_concrete : bool ref
  val wrap_constants : bool ref
end

(* for Parse *)
(* Whether the current file is an interface *)
val parse_intf : bool ref
