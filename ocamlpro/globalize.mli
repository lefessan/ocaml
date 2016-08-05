
val concrete:
  string -> Typedtree.structure -> Ident.t * Subst.t * Types.module_type

(* patch: internal hooks for typedtrees *)
(* by convention, rewriters should start with a two digit number "NN_name".
   90-99 are used for compilation rewriters. 80-89 for analysis rewriters.
   Code generation rewriters should be in 10-49.   *)
val add_internal_interface_rewriter : string ->
  (string -> Typedtree.signature -> Typedtree.signature) -> unit
val add_internal_implementation_rewriter : string ->
  (string -> Typedtree.structure -> Typedtree.structure) -> unit
val apply_internal_interface_rewriters :
  string -> Typedtree.signature -> Typedtree.signature
val apply_internal_implementation_rewriters :
  string -> Typedtree.structure * Typedtree.module_coercion ->
  Typedtree.structure * Typedtree.module_coercion
