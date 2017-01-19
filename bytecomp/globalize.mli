
val concrete:
  string -> Typedtree.structure -> Ident.t * Subst.t * Types.module_type

val init_location_table: string -> Typedtree.structure -> unit
val init_toplevel_location_table: Typedtree.structure -> unit
