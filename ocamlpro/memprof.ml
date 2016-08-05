(***********************************************************************)
(*                                                                     *)
(*                              ocp-memprof                            *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)


#if OCAML_VERSION = "4.01.0+ocp1"
module Bytes = String
    let string_of_bytes s = s
#else
    let string_of_bytes = Bytes.to_string
#endif

open Path
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

(* val block_info: t -> block_info array -> block_info array *)

(* Memprof *)

type alloc =
  | NoAlloc
  | LocId of int
type location = { l:Location.t; p: Path.t }

type delayed_alloc =
| Computed of alloc
| ToBeComputed of Location.t * Path.t * block_description
type locid = { loc: location; id:delayed_alloc ref }

(* Try to associate digests with source files. We use a cache of one filename
  since most locations are in the same source file. *)
let null_digest = Digest.from_hex (String.make 32 '0')
let src_digests = Hashtbl.create 13
let src_cached_filename = ref ""
let register_src loc =
  let name = loc.Location.loc_start.Lexing.pos_fname in
  if name != !src_cached_filename then begin
    if not (Hashtbl.mem src_digests name) then begin
      Hashtbl.add src_digests name
        (try
           Digest.file name
         with _ -> null_digest)
    end;
    src_cached_filename := name;
  end

(* Why use an array of 2^21 elements when we only plan to compile
a module with a few hundred allocation points ? Replaced by a Hashtbl. *)
let location_table = Hashtbl.create 113

(* When comparing types, [compare] can raise [Out_of_memory] for
   would-be identical types with different layouts. So, we implement a
   new hash table, catching [Out_of_memory] on [compare] and returning
   different locids in this case (fix a bug when compiling ocpweb). *)
module LimitedHashtbl = Hashtbl.Make(struct
  type t = block_info
  let hash = Hashtbl.hash
  let equal x y = try compare x y = 0 with Out_of_memory -> false
end)

let locids_table = LimitedHashtbl.create 113
  (* : block_description array = Array.create (1 lsl 21) Dummy *)
let location_pos = ref 0

let subst = ref Subst.identity
let data = ref (Ident.create_persistent "", Mty_signature [])
let globals = ref [||]

let init id mty s =
  if !Ocputils.Clflags.dump_concrete then
    Format.eprintf "%a@." Printtyp.modtype mty;
  data := (id, mty);
  subst := s;
  Hashtbl.clear location_table;
  LimitedHashtbl.clear locids_table;
(*  for i = 0 to !location_pos - 1 do
    location_table.(i) <- Dummy
  done; *)
  location_pos := 0;
  globals := [||]

let save_global_map map prims size =
  let t = Array.make size "?" in
  let t = Ident.fold_all (fun id (pos, _cc) t ->
    t.(pos) <- Ident.unique_name id;
    t) map t in
  List.iter (fun (pos, prim) ->
    t.(pos) <- Printf.sprintf "wrapper(%s)" (prim.Primitive.prim_name)
  ) prims;
  globals := t

let init_location_table module_name str =
  let (id, subst, mty) = Globalize.concrete module_name str in
  init id mty subst

let init_toplevel_location_table =
  let cpt = ref 0 in
  fun str ->
    incr cpt;
    let (id, subst, mty) =
      Globalize.concrete ("_TOP_" ^ string_of_int !cpt) str in
    init id mty subst

let init_package_location_table target_name =
  init target_name (Mty_signature []) Subst.identity

let get_alloc delayed =
  match !delayed with
  | Computed alloc -> alloc
  | ToBeComputed (l,p,desc) ->
    let alloc =
      let data = {
        block_loc = { block_location = l; block_path = Some p };
        block_desc = desc } in
      try
        LocId (LimitedHashtbl.find locids_table data)
      with Not_found ->
        let pos = !location_pos in
        incr location_pos;
        register_src l;
        Hashtbl.add location_table pos data;
        LimitedHashtbl.add locids_table data pos;
        LocId pos
    in
    delayed := Computed alloc;
    alloc

let get ~loc:l ~path:p desc =
  let id = ref (ToBeComputed (l, p, desc)) in
  { loc = {l; p}; id }

let min_pos (x: Lexing.position) (y: Lexing.position) =
  let open Lexing in
  assert (x.pos_fname = y.pos_fname);
  if x.pos_cnum < y.pos_cnum then x else y
let max_pos (x: Lexing.position) (y: Lexing.position) =
  let open Lexing in
  assert (x.pos_fname = y.pos_fname);
  if x.pos_cnum > y.pos_cnum then x else y

let merge_loc (a:Location.t) (b:Location.t) =
  let open Location in
  match (a,b) with
  | { loc_ghost=false; loc_start=a0; loc_end=a1 },
    { loc_ghost=false; loc_start=b0; loc_end=b1 } ->
      { loc_ghost = false;
        loc_start = min_pos a0 b0;
        loc_end = max_pos a1 b1; }
  | { loc_ghost = true }, _ -> { a with loc_end = b.loc_end }
  | _ ,{loc_ghost = true } -> { b with loc_start = a.loc_start }

let noalloc_computed = ref (Computed NoAlloc)
let noalloc l p = { loc = { l; p}; id = noalloc_computed }
let internal loc path s = get ~loc ~path (External (Internal s))
let cprim loc path s = get ~loc ~path (External (C s))
let wrapper loc path s = get ~loc ~path (External (Wrapper s))
let locid loc path ty = get ~loc ~path (Expr ty)
let mod_locid loc path ty = get ~loc ~path (Module ty)
let package_locid name =
  get ~loc:Location.none ~path:(Pident (Ident.create_persistent name)) Package
(*
let unit_locid name =
  get ~loc:Location.none ~path:(Pident (Ident.create_persistent name)) (Unit name)
*)
let recclosure_locid path l =
  match l with
  | [] -> assert false
  | [loc, ty] -> get ~loc ~path @@ Expr ty
  | ((loc, ty) :: locs) ->
      let loc = List.fold_left (fun l (l', _) -> merge_loc l l') loc locs in
      get ~loc ~path @@ RecClosure (ty :: List.map snd locs)
let class_locid loc path id = get ~loc ~path @@ Class (Pident id)
let anonymous_class_locid loc path id cs =
  get ~loc ~path @@ AnonymousClass (Pident id, cs)
let class_type_locid loc path id cty = get ~loc ~path @@ ClassType (Pident id, cty)
let inline_locid loc path base ofs = get ~loc ~path @@ Inlined (base, ofs)

module V0 = struct
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

module V1 = struct
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

type table_desc =
  | Indirect of string * string * int (* (filename, digest, table_size) *)
  | V0 of V0.module_info
  | Locid of string
  | V1 of V1.module_info

let input_table_desc ic = (input_value ic : table_desc)

exception Not_a_location_table of string
exception Unexpected_location_table of string * string

let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  let magic_number = Bytes.create len_magic_number in
  really_input ic magic_number 0 len_magic_number;
  string_of_bytes magic_number

let read_digest ic =
  let digest = Bytes.create 16 in
  really_input ic digest 0 16;
  string_of_bytes digest

let check_digest ?digest filename ic =
  let file_digest = read_digest ic in
  match digest with
  | None -> ()
  | Some digest ->
      if file_digest <> digest then
        raise (Unexpected_location_table (filename, file_digest))

let read ?digest filename =
  let ic = open_in_bin filename in
  try
    let magic_number = read_magic_number ic in
    if magic_number <> Config.cmg_magic_number then
      raise (Not_a_location_table filename);
    check_digest ?digest filename ic;
    seek_in ic (String.length Config.cmg_magic_number + 16);
    input_table_desc ic
  with e -> close_in ic; raise e

module DigestMap = Map.Make(String)

let read_hashes filenames =
  let rec loop map filenames =
    match filenames with
    | [] -> map
    | filename :: filenames ->
        let ic = open_in_bin filename in
        try
          let magic_number = read_magic_number ic in
          if magic_number <> Config.cmg_magic_number then
            (* TODO: Warn ?? *)
            invalid_arg "...";
          let file_digest = read_digest ic in
          close_in ic;
          loop (DigestMap.add file_digest filename map) filenames
        with e -> close_in ic; loop map filenames
  in
  loop DigestMap.empty filenames

let save filename info =
  let oc = open_out_bin filename in
  output_string oc Config.cmg_magic_number;
  output_string oc (String.make 16 '\000');
  output_value oc (info : table_desc);
  close_out oc;
  let digest = Digest.file filename in
  let oc = open_out_bin filename in
  seek_out oc (String.length Config.cmg_magic_number);
  output_string oc digest;
  close_out oc;
  digest

let dump_location_table ?filename locid_base =
  let table =
    Array.init !location_pos (fun i -> Hashtbl.find location_table i) in
(*  let table = Array.sub location_table 0 !location_pos in *)
  let srcs = ref [] in
  Hashtbl.iter (fun fname digest ->
    srcs := (fname, digest) :: !srcs
  ) src_digests;
  Hashtbl.clear src_digests; src_cached_filename := "";
  let info = V1.(V1 {
    locid_base = locid_base;
    modid = fst !data;
    modtype = snd !data;
    subst = !subst;
    loc_tbl = table;
    srcs = !srcs;
    pack_for = !Clflags.for_package;
    globals = !globals;
  })
  in
  let desc : table_desc =
    match filename with
    | _ when !Ocputils.Clflags.include_location_table -> info
    | None -> info
    | Some filename ->
        let full_path = Filename.concat (Sys.getcwd ()) filename in
        Indirect (full_path, save filename info, Array.length table)
  in
  (Marshal.to_string desc [], Array.length table)


module Subst = struct
  open Subst

  let block_description_raw s = function
    | Expr ty -> Expr (typexp s ty)
    | RecClosure tys -> RecClosure (List.map (typexp s) tys)
    | Class (p) -> Class (type_path s p)
    | AnonymousClass (p, cs) ->
      AnonymousClass (type_path s p, class_signature s cs)
    | ClassType (p, ct) ->
      ClassType (type_path s p, class_type s ct)
    | Dummy | External _ | Package
    | Unit _ | Module _ | Missing _ as data -> data
    | Inlined (base, ofs) -> Inlined (base, ofs)

  let block_description_raw' s = function
    | Module mty -> Module (modtype s mty)
    | data -> data

  let block_info_raw s info =
    { info with block_desc = block_description_raw s info.block_desc }
  let block_info_raw' s info =
    { info with block_desc = block_description_raw' s info.block_desc }

  let block_infos s l =
    let l = (Array.map (block_info_raw s) l) in
    Btype.cleanup_types ();
    Array.map (block_info_raw' s) l

    end
