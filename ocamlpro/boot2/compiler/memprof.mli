
open Types

(* WARNING: Modify byterun/memprof.c accordingly *)
type block_location =
  { block_location: Location.t;
    block_path: Path.t option;
  }

type alloc_reason =
| C of string
| Internal of string
| Wrapper of string

(* Modify byterun/memprof.c accordingly *)
type block_description =
  | Dummy
  | External of alloc_reason
  | Expr of type_expr
  | Module of module_type
  | Package
  | Unit of string
  | RecClosure of type_expr list
  | Class of Path.t
  | AnonymousClass of Path.t * class_signature
  | ClassType of Path.t * class_type
  | Missing of string * string
  | Inlined of string * int

(* Modify byterun/memprof.c accordingly *)
type block_info =
  { block_loc: block_location;
    block_desc: block_description }

type alloc =
  | NoAlloc
  | LocId of int
type location = { l:Location.t; p: Path.t }

type delayed_alloc
type locid = { loc: location; id:delayed_alloc ref }

val get_alloc : delayed_alloc ref -> alloc

val init : Ident.t * Subst.t * Types.module_type -> unit
val save_global_map :
  (int * Typedtree.module_coercion) Ident.tbl ->
  (int * Primitive.description) list -> int ->
  unit

val init_package_location_table: Ident.t -> unit

val internal : Location.t -> Path.t -> string -> locid
val cprim : Location.t -> Path.t -> string -> locid
val wrapper : Location.t -> Path.t -> string -> locid

val noalloc : Location.t -> Path.t -> locid
val locid: Location.t -> Path.t -> type_expr -> locid
val mod_locid: Location.t -> Path.t -> module_type -> locid
val package_locid: string -> locid
(*val unit_locid: string -> locid *)
val recclosure_locid:
  Path.t -> (Location.t * type_expr) list -> locid
val class_locid:
  Location.t -> Path.t -> Ident.t -> locid
val anonymous_class_locid:
  Location.t -> Path.t -> Ident.t -> class_signature -> locid
val class_type_locid:
  Location.t -> Path.t -> Ident.t -> class_type -> locid
val inline_locid:
  Location.t -> Path.t -> string -> int -> locid

val dump_location_table: (* ?filename:string -> *) string -> string * int

val location_none : location

module V0 : sig
(* WARNING: If you modify this type, make sure to modify function
 "register_externals" in "lib-headdump/c_common.c" that pre-allocates such
  a structure in C. **)
type module_info = {
  locid_base: string;
  modid: Ident.t;
  modtype: Types.module_type;
  subst: Subst.t;
  loc_tbl: block_info array;
  srcs : (string * Digest.t) list;
  globals : string array;
}
end

module V1 : sig
type module_info = {
  locid_base: string;
  modid: Ident.t;
  modtype: Types.module_type;
  subst: Subst.t;
  loc_tbl: block_info array;
  srcs : (string * Digest.t) list;
  globals : string array;
  pack_for : string option;
}
end

(* Keep this order. V0 is used in C (c_common.c). If you want to add new
fields, you should define a new module and a new constructor, to remain
  backward compatible with dumps. *)
type table_desc =
  | Indirect of string * string * int (* (filename, digest, table_size) *)
  | V0 of V0.module_info
  | Locid of string
  | V1 of V1.module_info

module Subst : sig

  val block_infos : Subst.t -> block_info array -> block_info array

end
val nolocid : locid
