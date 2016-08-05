(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let prim_memprof = false

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Unknown_builtin_primitive of string

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun cc rootpath id modl -> assert false) :
      module_coercion -> Lambda.location -> Ident.t option -> module_expr -> lambda)

let transl_object =
  ref (fun locid id s cl -> assert false :
       Lambda.locid -> Ident.t -> string list -> class_expr -> lambda)

(* Translation of primitives *)

let prim_alloc name arity =
  let prim_memprof = false in
  { prim_name = name; prim_arity = arity;
    prim_alloc = true;
    prim_native_name = ""; prim_native_float = false}
let prim_noalloc name arity =
  { (prim_alloc name arity) with prim_alloc = false; }
let prim_memprof name arity =
  let prim_memprof = true in
  { (prim_alloc name arity) with prim_memprof; }

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall( prim_alloc "caml_equal" 2, None),
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pccall( prim_noalloc "caml_string_equal" 2 , None),
       Pbintcomp(Pnativeint, Ceq),
       Pbintcomp(Pint32, Ceq),
       Pbintcomp(Pint64, Ceq),
       true);
  "%notequal",
      (Pccall( prim_alloc "caml_notequal" 2 , None),
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pccall( prim_noalloc "caml_string_notequal" 2 , None),
       Pbintcomp(Pnativeint, Cneq),
       Pbintcomp(Pint32, Cneq),
       Pbintcomp(Pint64, Cneq),
       true);
  "%lessthan",
      (Pccall( prim_alloc "caml_lessthan" 2 , None),
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pccall( prim_noalloc "caml_string_lessthan" 2 , None),
       Pbintcomp(Pnativeint, Clt),
       Pbintcomp(Pint32, Clt),
       Pbintcomp(Pint64, Clt),
       false);
  "%greaterthan",
      (Pccall( prim_alloc "caml_greaterthan" 2 , None),
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pccall( prim_noalloc "caml_string_greaterthan" 2 , None),
       Pbintcomp(Pnativeint, Cgt),
       Pbintcomp(Pint32, Cgt),
       Pbintcomp(Pint64, Cgt),
       false);
  "%lessequal",
      (Pccall( prim_alloc "caml_lessequal" 2 , None),
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pccall( prim_noalloc "caml_string_lessequal" 2 , None),
       Pbintcomp(Pnativeint, Cle),
       Pbintcomp(Pint32, Cle),
       Pbintcomp(Pint64, Cle),
       false);
  "%greaterequal",
      (Pccall( prim_alloc "caml_greaterequal" 2 , None),
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pccall( prim_alloc "caml_string_greaterequal" 2 , None),
       Pbintcomp(Pnativeint, Cge),
       Pbintcomp(Pint32, Cge),
       Pbintcomp(Pint64, Cge),
       false);
  "%compare",
      (Pccall( prim_alloc "caml_compare" 2 , None),
       Pccall( prim_noalloc "caml_int_compare" 2 , None),
       Pccall( prim_noalloc "caml_float_compare" 2 , None),
       Pccall( prim_noalloc "caml_string_compare" 2 , None),
       Pccall( prim_noalloc "caml_nativeint_compare" 2 , None),
       Pccall( prim_noalloc "caml_int32_compare" 2 , None),
       Pccall( prim_noalloc "caml_int64_compare" 2 , None),
       false)
]

let primitives_table = create_hashtable 57 [
  "%identity", Pidentity;
  "%ignore", Pignore;
  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield(0, true);
  "%raise", Praise Raise_regular;
  "%reraise", Praise Raise_reraise;
  "%raise_notrace", Praise Raise_notrace;
  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;
  "%big_endian", Pctconst Big_endian;
  "%word_size", Pctconst Word_size;
  "%ostype_unix", Pctconst Ostype_unix;
  "%ostype_win32", Pctconst Ostype_win32;
  "%ostype_cygwin", Pctconst Ostype_cygwin;
  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint;
  "%modint", Pmodint;
  "%andint", Pandint;
  "%orint", Porint;
  "%xorint", Pxorint;
  "%lslint", Plslint;
  "%lsrint", Plsrint;
  "%asrint", Pasrint;
  "%eq", Pintcomp Ceq;
  "%noteq", Pintcomp Cneq;
  "%ltint", Pintcomp Clt;
  "%leint", Pintcomp Cle;
  "%gtint", Pintcomp Cgt;
  "%geint", Pintcomp Cge;
  "%incr", Poffsetref(1);
  "%decr", Poffsetref(-1);
  "%intoffloat", Pintoffloat;
  "%eqfloat", Pfloatcomp Ceq;
  "%noteqfloat", Pfloatcomp Cneq;
  "%ltfloat", Pfloatcomp Clt;
  "%lefloat", Pfloatcomp Cle;
  "%gtfloat", Pfloatcomp Cgt;
  "%gefloat", Pfloatcomp Cge;
  "%string_length", Pstringlength;
  "%string_safe_get", Pstringrefs;
  "%string_safe_set", Pstringsets;
  "%string_unsafe_get", Pstringrefu;
  "%string_unsafe_set", Pstringsetu;
  "%array_length", Parraylength Pgenarray;
  "%array_safe_set", Parraysets Pgenarray;
  "%array_unsafe_set", Parraysetu Pgenarray;
  "%obj_size", Parraylength Pgenarray;
  "%obj_set_field", Parraysetu Pgenarray;
  "%obj_is_int", Pisint;
  "%lazy_force", Plazyforce;
  "%nativeint_to_int", Pintofbint Pnativeint;
  "%int32_to_int", Pintofbint Pint32;
  "%int64_to_int", Pintofbint Pint64;
  "%caml_ba_set_1",
    Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_2",
    Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_3",
    Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_1",
    Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_2",
    Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_3",
    Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_dim_1", Pbigarraydim(1);
  "%caml_ba_dim_2", Pbigarraydim(2);
  "%caml_ba_dim_3", Pbigarraydim(3);
  "%caml_string_set16", Pstring_set_16(false);
  "%caml_string_set16u", Pstring_set_16(true);
  "%caml_string_set32", Pstring_set_32(false);
  "%caml_string_set32u", Pstring_set_32(true);
  "%caml_string_set64", Pstring_set_64(false);
  "%caml_string_set64u", Pstring_set_64(true);
  "%caml_bigstring_get16", Pbigstring_load_16(false);
  "%caml_bigstring_get16u", Pbigstring_load_16(true);
  "%caml_bigstring_set16", Pbigstring_set_16(false);
  "%caml_bigstring_set16u", Pbigstring_set_16(true);
  "%caml_bigstring_set32", Pbigstring_set_32(false);
  "%caml_bigstring_set32u", Pbigstring_set_32(true);
  "%caml_bigstring_set64", Pbigstring_set_64(false);
  "%caml_bigstring_set64u", Pbigstring_set_64(true);
  "%bswap16", Pbswap16;
  "%int_as_pointer", Pint_as_pointer;
]

let mklocid loc ty = Memprof.locid loc.l loc.p ty
let primitives_with_loc_table = create_hashtable 57 [
  "%makeblock",
  (fun loc ->
    Pmakeblock(0, Immutable,
               Memprof.cprim loc.l loc.p "%makeblock"));
  "%makemutable",
  (fun loc ->
    Pmakeblock(0, Mutable,
               Memprof.cprim loc.l loc.p "%makemutable"));

  "%nativeint_of_int",
   (fun loc -> Pbintofint (Pnativeint,
                  mklocid loc Predef.type_nativeint));
  "%nativeint_neg",
    (fun loc ->  Pnegbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_add",
    (fun loc ->  Paddbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_sub",
    (fun loc ->  Psubbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_mul",
    (fun loc ->  Pmulbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_div",
    (fun loc ->  Pdivbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_mod",
    (fun loc ->  Pmodbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_and",
    (fun loc ->  Pandbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_or",
    (fun loc ->   Porbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_xor",
    (fun loc ->  Pxorbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_lsl",
    (fun loc ->  Plslbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_lsr",
    (fun loc ->  Plsrbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%nativeint_asr",
    (fun loc ->  Pasrbint (Pnativeint,
                 mklocid loc Predef.type_nativeint));
  "%int32_of_int",
    (fun loc ->  Pbintofint (Pint32, mklocid loc Predef.type_int32));
  "%int32_neg",
    (fun loc ->  Pnegbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_add",
    (fun loc ->  Paddbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_sub",
    (fun loc ->  Psubbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_mul",
    (fun loc ->  Pmulbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_div",
    (fun loc ->  Pdivbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_mod",
    (fun loc ->  Pmodbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_and",
    (fun loc ->  Pandbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_or",
    (fun loc ->   Porbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_xor",
    (fun loc ->  Pxorbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_lsl",
    (fun loc ->  Plslbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_lsr",
    (fun loc ->  Plsrbint (Pint32, mklocid loc Predef.type_int32));
  "%int32_asr",
    (fun loc ->  Pasrbint (Pint32, mklocid loc Predef.type_int32));
  "%int64_of_int",
    (fun loc ->  Pbintofint (Pint64, mklocid loc Predef.type_int64));
  "%int64_neg",
    (fun loc ->  Pnegbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_add",
    (fun loc ->  Paddbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_sub",
    (fun loc ->  Psubbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_mul",
    (fun loc ->  Pmulbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_div",
    (fun loc ->  Pdivbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_mod",
    (fun loc ->  Pmodbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_and",
    (fun loc ->  Pandbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_or",
    (fun loc ->   Porbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_xor",
    (fun loc ->  Pxorbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_lsl",
    (fun loc ->  Plslbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_lsr",
    (fun loc ->  Plsrbint (Pint64, mklocid loc Predef.type_int64));
  "%int64_asr",
    (fun loc ->  Pasrbint (Pint64, mklocid loc Predef.type_int64));
  "%nativeint_of_int32",
    (fun loc ->  Pcvtbint(Pint32, Pnativeint,
                mklocid loc Predef.type_nativeint));
  "%nativeint_to_int32",
    (fun loc ->  Pcvtbint(Pnativeint, Pint32,
                mklocid loc Predef.type_int32));
  "%int64_of_int32",
    (fun loc ->  Pcvtbint(Pint32, Pint64,
                mklocid loc Predef.type_int64));
  "%int64_to_int32",
    (fun loc ->  Pcvtbint(Pint64, Pint32,
                mklocid loc Predef.type_int32));
  "%int64_of_nativeint",
      (fun loc ->  Pcvtbint(Pnativeint, Pint64,
                              mklocid loc Predef.type_int64));
  "%int64_to_nativeint",
      (fun loc ->  Pcvtbint(Pint64, Pnativeint,
                                mklocid loc Predef.type_nativeint));
  "%bswap_int32",
    (fun loc ->  Pbbswap(Pint32, mklocid loc Predef.type_int32));
  "%bswap_int64",
    (fun loc ->  Pbbswap(Pint64, mklocid loc Predef.type_int64));
  "%bswap_native",
    (fun loc ->  Pbbswap(Pnativeint, mklocid loc Predef.type_nativeint));
  "%floatofint",
    (fun loc ->  Pfloatofint (mklocid loc Predef.type_float));
  "%negfloat",
    (fun loc ->  Pnegfloat (mklocid loc Predef.type_float));
  "%absfloat",
    (fun loc ->  Pabsfloat (mklocid loc Predef.type_float));
  "%addfloat",
    (fun loc ->  Paddfloat (mklocid loc Predef.type_float));
  "%subfloat",
    (fun loc ->  Psubfloat (mklocid loc Predef.type_float));
  "%mulfloat",
    (fun loc ->  Pmulfloat (mklocid loc Predef.type_float));
  "%divfloat",
    (fun loc ->  Pdivfloat (mklocid loc Predef.type_float));
  "%array_safe_get",
    (fun loc ->  Parrayrefs (Pgenarray, mklocid loc Predef.type_float));
  "%array_unsafe_get",
    (fun loc ->  Parrayrefu (Pgenarray, mklocid loc Predef.type_float));
  "%obj_field",
    (fun loc ->  Parrayrefu (Pgenarray, mklocid loc Predef.type_float));
  "%caml_ba_ref_1",
    (fun loc -> Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout, loc));
  "%caml_ba_ref_2",
    (fun loc -> Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout, loc));
  "%caml_ba_ref_3",
    (fun loc -> Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout, loc));
  "%caml_ba_unsafe_ref_1",
    (fun loc -> Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout, loc));
  "%caml_ba_unsafe_ref_2",
    (fun loc -> Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout, loc));
  "%caml_ba_unsafe_ref_3",
    (fun loc -> Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout, loc));
   "%caml_string_get16", (fun loc -> Pstring_load_16(false));
  "%caml_string_get16u", (fun loc -> Pstring_load_16(true));
  "%caml_string_get32", (fun loc -> Pstring_load_32(false, loc));
  "%caml_string_get32u", (fun loc -> Pstring_load_32(true, loc));
  "%caml_string_get64", (fun loc -> Pstring_load_64(false, loc));
  "%caml_string_get64u", (fun loc -> Pstring_load_64(true, loc));
  "%caml_bigstring_get32", (fun loc -> Pbigstring_load_32(false, loc));
  "%caml_bigstring_get32u", (fun loc -> Pbigstring_load_32(true, loc));
  "%caml_bigstring_get64", (fun loc -> Pbigstring_load_64(false, loc));
  "%caml_bigstring_get64u", (fun loc -> Pbigstring_load_64(true, loc));

]

let prim_makearray = prim_memprof "caml_make_vect" 2
let prim_obj_dup = prim_alloc "caml_obj_dup" 1


let find_primitive loc prim_name =
  match prim_name with
      "%revapply" -> Prevapply loc
    | "%apply" -> Pdirapply loc
    | "%loc_LOC" -> Ploc Loc_LOC
    | "%loc_FILE" -> Ploc Loc_FILE
    | "%loc_LINE" -> Ploc Loc_LINE
    | "%loc_POS" -> Ploc Loc_POS
    | "%loc_MODULE" -> Ploc Loc_MODULE
    | name ->
      try Hashtbl.find primitives_table prim_name
      with Not_found ->
        Hashtbl.find primitives_with_loc_table prim_name loc

let transl_prim loc prim args =
  let prim_name = prim.prim_name in
  try
    let (gencomp, intcomp, floatcomp, stringcomp,
         nativeintcomp, int32comp, int64comp,
         simplify_constant_constructor) =
      Hashtbl.find comparisons_table prim_name in
    begin match args with
      [arg1; {exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}; arg2]
      when simplify_constant_constructor ->
        intcomp
    | [arg1; {exp_desc = Texp_variant(_, None)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Texp_variant(_, None)}; exp2]
      when simplify_constant_constructor ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int
                     || has_base_type arg1 Predef.path_char ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_float ->
        floatcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_string ->
        stringcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_nativeint ->
        nativeintcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int32 ->
        int32comp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int64 ->
        int64comp
    | _ ->
        gencomp
    end
  with Not_found ->
  try
    let p = find_primitive loc prim_name in
    (* Try strength reduction based on the type of the argument *)
    begin match (p, args) with
        (Psetfield(n, _), [arg1; arg2]) -> Psetfield(n, maybe_pointer arg2)
      | (Parraylength Pgenarray, [arg])   -> Parraylength(array_kind arg)
      | (Parrayrefu (Pgenarray, locid), arg1 :: _) ->
          Parrayrefu(array_kind arg1, locid)
      | (Parraysetu Pgenarray, arg1 :: _) -> Parraysetu(array_kind arg1)
      | (Parrayrefs (Pgenarray, locid), arg1 :: _) ->
          Parrayrefs(array_kind arg1, locid)
      | (Parraysets Pgenarray, arg1 :: _) -> Parraysets(array_kind arg1)
      | (Pbigarrayref(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout, loc),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayref(unsafe, n, k, l, loc)
      | (Pbigarrayset(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayset(unsafe, n, k, l)
      | _ -> p
    end
  with Not_found ->
    if String.length prim_name > 0 && prim_name.[0] = '%' then
      raise(Error(loc.l, Unknown_builtin_primitive prim_name));
    Pccall (prim, None)

(* Extend [transl_prim] with [locid] inherited from the calling
   context:
   * if "!memprof" was provided, the result as the type of [expr_locid]
   * if "float" was provided, the result is unboxed, but might be
     immediatly boxed, we use the type of [expr_locid] also here
   * otherwise, if the primitive allocates, all allocations are
     just marked as "external", with the location of the calling position.
 *)

let transl_prim locid prim args =
  match transl_prim locid.loc prim args with
  | Pccall (
    (   { prim_alloc = true; prim_memprof = true; }
      | { prim_native_float = true } )
      as p, None) ->
    Pccall (p, Some locid)
  | Pccall ({ prim_alloc = true } as p, None) ->
    Pccall (p, Some (Memprof.cprim locid.loc.l locid.loc.p p.prim_name))
  | prim -> prim

(* Eta-expand a primitive without knowing the types of its arguments *)

let transl_primitive loc p =
  let prim =
    try
      let (gencomp, _, _, _, _, _, _, _) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      find_primitive loc p.prim_name
    with Not_found ->
      Pccall (p, None) in
  match prim with
  | Plazyforce ->
      let parm = Ident.create "prim" in
      Lfunction(Curried, [parm],
                 (* is the locid here correct ? the type is probably the one
                    of the englobing expression, no ? *)
                 Matching.inline_lazy_force (Lvar parm) loc,
                 Memprof.wrapper loc.l loc.p "%lazy_force")
  | Ploc kind ->
    let lam = lam_of_loc kind loc.l in
    begin match p.prim_arity with
      | 0 -> lam
      | 1 -> (* TODO: we should issue a warning ? *)
        let param = Ident.create "prim" in
        Lfunction(Curried, [param],
          Lprim(Pmakeblock(0, Immutable,
                           Memprof.cprim loc.l loc.p "%loc"),
                [lam; Lvar param]),
                  Memprof.wrapper loc.l loc.p "%loc")
      | _ -> assert false
    end
  | _ ->
    let prim = match prim with
        Pccall ({ prim_alloc = true } as p, None) ->
        Pccall (p, Some (Memprof.cprim loc.l loc.p p.prim_name))
      | prim -> prim
    in
      let rec make_params n =
        if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
      let params = make_params p.prim_arity in
      Lfunction(Curried, params,
      Lprim(prim, List.map (fun id -> Lvar id) params),
              Memprof.wrapper loc.l loc.p p.prim_name )

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda idlist lam =
  let rec check_top idlist = function
    | Lvar v -> not (List.mem v idlist)
    | Llet (_, _, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, loc, arg, body) ->
        check idlist arg && check_top (add_let id arg idlist) body
    | Lletrec(bindings, body, _) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check_top idlist' body
    | Lprim (Pmakearray (Pgenarray, _), args) -> false
    | Lsequence (lam1, lam2) -> check idlist lam1 && check_top idlist lam2
    | Levent (lam, _) -> check_top idlist lam
    | lam -> check idlist lam

  and check idlist = function
    | Lvar _ -> true
    | Lfunction(kind, params, body, _) -> true
    | Llet (_, _, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, loc, arg, body) ->
        check idlist arg && check (add_let id arg idlist) body
    | Lletrec(bindings, body, _) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check idlist' body
    | Lprim(Pmakeblock(tag, mut, _), args) ->
        List.for_all (check idlist) args
    | Lprim(Pmakearray(_,_), args) ->
        List.for_all (check idlist) args
    | Lsequence (lam1, lam2) -> check idlist lam1 && check idlist lam2
    | Levent (lam, _) -> check idlist lam
    | lam ->
        let fv = free_variables lam in
        not (List.exists (fun id -> IdentSet.mem id fv) idlist)

  and add_let id arg idlist =
    let fv = free_variables arg in
    if List.exists (fun id -> IdentSet.mem id fv) idlist
    then id :: idlist
    else idlist

  and add_letrec bindings idlist =
    List.fold_right (fun (id, arg) idl -> add_let id arg idl)
                    bindings idlist

  (* reverse-engineering the code generated by transl_record case 2 *)
  (* If you change this, you probably need to change Bytegen.size_of_lambda. *)
  and check_recursive_recordwith idlist = function
    | Llet (Strict, id1, _, Lprim (Pduprecord _, [e1]), body) ->
       check_top idlist e1
       && check_recordwith_updates idlist id1 body
    | _ -> false

  and check_recordwith_updates idlist id1 = function
    | Lsequence (Lprim ((Psetfield _ | Psetfloatfield _), [Lvar id2; e1]), cont)
        -> id2 = id1 && check idlist e1
           && check_recordwith_updates idlist id1 cont
    | Lvar id2 -> id2 = id1
    | _ -> false

  in check_top idlist lam

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> Ident.create default
  | {c_lhs=p; _} as case :: rem ->
      match p.pat_desc with
        Tpat_var (id, _) -> id
      | Tpat_alias(p, id, _) -> id
      | Tpat_lazy c_lhs -> name_pattern default ({case with c_lhs} :: rem)
      | _ -> name_pattern default rem

let field_path loc p =
  let rec name p =
    match p.pat_desc with
    | Tpat_var (id, _) | Tpat_alias(_, id, _) -> Ident.name id
    | Tpat_lazy p -> name p
    | Tpat_tuple [] | Tpat_construct (_,_,[]) ->
        let loc = p.pat_loc.Location.loc_start in
        let line = loc.Lexing.pos_lnum in
        let char = loc.Lexing.pos_cnum - loc.Lexing.pos_bol in
        Printf.sprintf "unit_%d_%d" line char
    | Tpat_tuple ps | Tpat_construct (_,_,ps) ->
        String.concat "," (List.map name ps)
    | _ -> raise Not_found in
  let name = try name p with Not_found -> "(*_complex_pattern_*)" in
  { loc with p = Path.Pdot(loc.p, name, Path.nopos) }

let rec_field_path loc idlist =
  { loc with p =
  Path.Pdot (loc.p, String.concat "-" (List.map Ident.name idlist), Path.nopos)
  }
(* Push the default values under the functional abstractions *)

let rec push_defaults loc bindings cases partial =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function(l, pl,partial)} as exp}] ->
      let pl = push_defaults exp.exp_loc bindings pl partial in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc = Texp_function(l, pl, partial)}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{txt="#default"},_];
             exp_desc = Texp_let
               (Nonrecursive, binds, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (binds :: bindings) [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                    partial
  | [case] ->
      let exp =
        List.fold_left
          (fun exp binds ->
            {exp with exp_desc = Texp_let(Nonrecursive, binds, exp)})
          case.c_rhs bindings
      in
      [{case with c_rhs=exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let param = name_pattern "param" cases in
      let name = Ident.name param in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param, mknoloc (Longident.Lident name),
                          {val_type = pat.pat_type; val_kind = Val_reg;
                           val_attributes = [];
                           Types.val_loc = Location.none;
                          })},
             cases, [], partial) }
      in
      push_defaults loc bindings
        [{c_lhs={pat with pat_desc = Tpat_var (param, mknoloc name)};
          c_guard=None; c_rhs=exp}]
        Total
  | _ ->
      cases

(* Insertion of debugging events *)

let event_before exp lam = match lam with
| Lstaticraise (_,_,_) -> lam
| _ ->
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_after exp lam =
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_function exp lam =
  if !Clflags.debug then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = Env.summary exp.exp_env}))
  else
    lam None

let primitive_is_ccall = function
  (* Determine if a primitive is a Pccall or will be turned later into
     a C function call that may raise an exception *)
  | Pccall _ | Pstringrefs | Pstringsets | Parrayrefs _ | Parraysets _ |
    Pbigarrayref _ | Pbigarrayset _ | Pduprecord _ -> true
  | _ -> false

(* Assertions *)

let assert_failed exp =
  let (fname, line, char) =
    Location.get_pos_info exp.exp_loc.Location.loc_start in
  Lprim(Praise Raise_regular, [event_after exp
    (Lprim(Pmakeblock(0, Immutable,
           Memprof.locid exp.exp_loc Predef.path_exn Predef.type_exn),
          [transl_normal_path Predef.path_assert_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string (fname, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))]))])
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let try_ids = Hashtbl.create 8

let rec transl_exp loc e =
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 loc e else
  Translobj.oo_wrap (newl loc e.exp_loc) e.exp_env true (transl_exp0 loc) e

and transl_exp0 loc e =
  let loc = newl loc e.exp_loc in
  let expr_locid = Memprof.locid loc.l loc.p e.exp_type in
  match e.exp_desc with
    Texp_ident(path, _, {val_kind = Val_prim p}) ->
      let public_send = p.prim_name = "%send" in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        Lfunction(Curried, [obj; meth], Lsend(kind, Lvar meth, Lvar obj, [],
                                              loc),
                  expr_locid)
      else if p.prim_name = "%sendcache" then
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        let cache = Ident.create "cache" and pos = Ident.create "pos" in
        Lfunction(Curried, [obj; meth; cache; pos],
                  Lsend(Cached, Lvar meth, Lvar obj, [Lvar cache; Lvar pos],
                        loc),
                  expr_locid)
      else
        transl_primitive loc p
  | Texp_ident(path, _, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, _, {val_kind = Val_reg | Val_self _}) ->
      transl_path ~loc:e.exp_loc e.exp_env path
  | Texp_ident _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let loc rec_flag pat_expr_list
                 (event_before body (transl_exp loc body))
  | Texp_function (_, pat_expr_list, partial) ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            let pl = push_defaults e.exp_loc [] pat_expr_list partial in
            transl_function
              loc !Clflags.native_code repr partial pl)
      in
      Lfunction(kind, params, body, expr_locid)
  | Texp_apply({exp_desc = Texp_ident(path, _, {val_kind = Val_prim p})},
               oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (_, arg,_) -> arg <> None) oargs ->
      let args, args' = cut p.prim_arity oargs in
      let wrap f =
        if args' = []
        then event_after e f
        else event_after e (transl_apply loc f args')
      in
      let wrap0 f =
        if args' = [] then f else wrap f in
      let args =
         List.map (function _, Some x, _ -> x | _ -> assert false) args in
      let argl = transl_list loc args in
      let public_send = p.prim_name = "%send"
        || not !Clflags.native_code && p.prim_name = "%sendcache"in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = List.hd argl in
        wrap (Lsend (kind, List.nth argl 1, obj, [], loc))
      else if p.prim_name = "%sendcache" then
        match argl with [obj; meth; cache; pos] ->
          wrap (Lsend(Cached, meth, obj, [cache; pos], loc))
        | _ -> assert false
      else begin
        let prim = transl_prim (* e.exp_loc *) expr_locid p args in
        match (prim, args) with
          (Praise k, [arg1]) ->
            let targ = List.hd argl in
            let k =
              match k, targ with
              | Raise_regular, Lvar id
                when Hashtbl.mem try_ids id ->
                  Raise_reraise
              | _ ->
                  k
            in
            wrap0 (Lprim(Praise k, [event_after arg1 targ]))
        | (Ploc kind, []) ->
          lam_of_loc kind e.exp_loc
        | (Ploc kind, [arg1]) ->
          let lam = lam_of_loc kind arg1.exp_loc in
          Lprim(Pmakeblock(0, Immutable, expr_locid), lam :: argl)
        | (Ploc _, _) -> assert false
        | (_, _) ->
            begin match (prim, argl) with
            | (Plazyforce, [a]) ->
                wrap (Matching.inline_lazy_force a loc)
            | (Plazyforce, _) -> assert false
            |_ -> let p = Lprim(prim, argl) in
               if primitive_is_ccall prim then wrap p else wrap0 p
            end
      end
  | Texp_apply(funct, oargs) ->
      event_after e (transl_apply loc (transl_exp loc funct) oargs)
  | Texp_match(arg, pat_expr_list, exn_pat_expr_list, partial) ->
    transl_match loc e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = name_pattern "exn" pat_expr_list in
      Ltrywith(transl_exp loc body, id,
               Matching.for_trywith loc (Lvar id) (transl_cases_try loc pat_expr_list))
  | Texp_tuple el ->
      let ll = transl_list loc el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, expr_locid), ll)
      end
  | Texp_construct(_, cstr, args) ->
      let ll = transl_list loc args in
      begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(Const_pointer n)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable, expr_locid), ll)
          end
      | Cstr_extension(path, is_const) ->
          if is_const then
            transl_path e.exp_env path
          else
            Lprim(Pmakeblock(0, Immutable, expr_locid),
                  transl_path e.exp_env path :: ll)
      end
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(Const_pointer tag)
      | Some arg ->
          let lam = transl_exp loc arg in
          try
            Lconst(Const_block(0, [Const_base(Const_int tag);
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, expr_locid),
                  [Lconst(Const_base(Const_int tag)); lam])
      end
  | Texp_record ((_, lbl1, _) :: _ as lbl_expr_list, opt_init_expr) ->
      transl_record expr_locid
        lbl1.lbl_all lbl1.lbl_repres lbl_expr_list opt_init_expr
  | Texp_record ([], _) ->
      fatal_error "Translcore.transl_exp: bad Texp_record"
  | Texp_field(arg, lid, lbl) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Pfield lbl.lbl_pos
        | Record_float ->
            Pfloatfield (lbl.lbl_pos,
                         Memprof.locid lid.loc loc.p Predef.type_float) in
      Lprim(access, [transl_exp loc arg])
  | Texp_setfield(arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      Lprim(access, [transl_exp loc arg; transl_exp loc newval])
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list loc expr_list in
      begin try
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= 4 then raise Not_constant;
        let cl = List.map extract_constant ll in
        let master =
          match kind with
          | Paddrarray | Pintarray ->
              Lconst(Const_block(0, cl))
          | Pfloatarray ->
              Lconst(Const_float_array(List.map extract_float cl))
          | Pgenarray ->
              raise Not_constant in             (* can this really happen? *)
        Lprim(Pccall (prim_obj_dup, Some expr_locid), [master])
      with Not_constant ->
        Lprim(Pmakearray (kind, expr_locid), ll)
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp loc cond,
                  event_before ifso (transl_exp loc ifso),
                  event_before ifnot (transl_exp loc ifnot))
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp loc cond,
                  event_before ifso (transl_exp loc ifso),
                  lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp loc expr1, event_before expr2 (transl_exp loc expr2))
  | Texp_while(cond, body) ->
      Lwhile(transl_exp loc cond, event_before body (transl_exp loc body))
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp loc low, transl_exp loc high, dir,
           event_before body (transl_exp loc body))
  | Texp_send(_, _, Some exp) -> transl_exp loc exp
  | Texp_send(expr, met, None) ->
      let obj = transl_exp loc expr in
      let lam =
        match met with
          Tmeth_val id -> Lsend (Self, Lvar id, obj, [], loc)
        | Tmeth_name nm ->
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, loc)
      in
      event_after e lam
  | Texp_new (cl, {Location.loc=lloc}, _) ->
      Lapply(Lprim(Pfield 0, [transl_path ~loc:lloc e.exp_env cl]),
             [lambda_unit], loc)
  | Texp_instvar(path_self, path, _) ->
      Lprim(Parrayrefu (Paddrarray, Memprof.noalloc loc.l loc.p),
            [transl_normal_path path_self; transl_normal_path path])
  | Texp_setinstvar(path_self, path, _, expr) ->
      transl_setinstvar loc (transl_normal_path path_self) path expr
  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      Llet(Strict, cpy, loc,
           Lapply(Translobj.oo_prim "copy", [transl_normal_path path_self],
                  loc),
           List.fold_right
             (fun (path, _, expr) rem ->
                Lsequence(transl_setinstvar loc (Lvar cpy) path expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(id, _, modl, body) ->
    Llet(Strict, id, loc,
         !transl_module Tcoerce_none loc None modl, transl_exp loc body)
  | Texp_pack modl ->
      !transl_module Tcoerce_none loc None modl
  | Texp_assert {exp_desc=Texp_construct(_, {cstr_name="false"}, _)} ->
      assert_failed e
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp loc cond, lambda_unit, assert_failed e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match e.exp_desc with
        (* a constant expr of type <> float gets compiled as itself *)
      | Texp_constant
          ( Const_int _ | Const_char _ | Const_string _
          | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
      | Texp_function(_, _, _)
      | Texp_construct (_, {cstr_arity = 0}, _)
        -> transl_exp loc e
      | Texp_constant(Const_float _) ->
          Lprim(Pmakeblock(Obj.forward_tag, Immutable,
                           expr_locid),
                [transl_exp loc e])
      | Texp_ident(_, _, _) -> (* according to the type *)
          begin match e.exp_type.desc with
          (* the following may represent a float/forward/lazy: need a
             forward_tag *)
          | Tvar _ | Tlink _ | Tsubst _ | Tunivar _
          | Tpoly(_,_) | Tfield(_,_,_,_) ->
              Lprim(Pmakeblock(Obj.forward_tag, Immutable,
                               expr_locid),
                    [transl_exp loc e])
          (* the following cannot be represented as float/forward/lazy:
             optimize *)
          | Tarrow(_,_,_,_) | Ttuple _ | Tpackage _ | Tobject(_,_) | Tnil
          | Tvariant _
              -> transl_exp loc e
          (* optimize predefined types (excepted float) *)
          | Tconstr(_,_,_) ->
              if has_base_type e Predef.path_int
                || has_base_type e Predef.path_char
                || has_base_type e Predef.path_string
                || has_base_type e Predef.path_bool
                || has_base_type e Predef.path_unit
                || has_base_type e Predef.path_exn
                || has_base_type e Predef.path_array
                || has_base_type e Predef.path_list
                || has_base_type e Predef.path_option
                || has_base_type e Predef.path_nativeint
                || has_base_type e Predef.path_int32
                || has_base_type e Predef.path_int64
              then transl_exp loc e
              else
                Lprim(Pmakeblock(Obj.forward_tag, Immutable, expr_locid), [transl_exp loc e])
          end
      (* other cases compile to a lazy block holding a function *)
      | _ ->
          let fn = Lfunction (Curried, [Ident.create "param"], transl_exp loc e, expr_locid) in
          Lprim(Pmakeblock(Config.lazy_tag, Mutable, expr_locid), [fn])
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create "class" in
      !transl_object
        (Memprof.anonymous_class_locid loc.l loc.p cl cty)
        cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }

and transl_list loc expr_list =
  List.map (transl_exp loc) expr_list

and transl_guard loc guard rhs =
  let expr = event_before rhs (transl_exp loc rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before cond (Lifthenelse(transl_exp loc cond, expr, staticfail loc))

and transl_case loc {c_lhs; c_guard; c_rhs} =
  c_lhs, transl_guard loc c_guard c_rhs

and transl_cases loc cases =
  List.map (transl_case loc) cases

and transl_case_try loc {c_lhs; c_guard; c_rhs} =
  match c_lhs.pat_desc with
  | Tpat_var (id, _)
  | Tpat_alias (_, id, _) ->
      Hashtbl.replace try_ids id ();
      Misc.try_finally
        (fun () -> c_lhs, transl_guard loc c_guard c_rhs)
        (fun () -> Hashtbl.remove try_ids id)
  | _ ->
      c_lhs, transl_guard loc c_guard c_rhs

and transl_cases_try loc cases =
  List.map (transl_case_try loc) cases

and transl_tupled_cases loc patl_expr_list =
  List.map (fun (patl, guard, expr) -> (patl, transl_guard loc guard expr))
    patl_expr_list

and transl_apply loc lam sargs =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs, loc) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Levent(Lsend(k, lmet, lobj, largs, loc), _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Lapply(lexp, largs, loc) ->
        Lapply(lexp, largs @ args, loc)
    | lexp ->
        Lapply(lexp, args, loc)
  in
  let rec build_apply lam args = function
      (None, optional) :: l ->
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        let args, args' =
          if List.for_all (fun (_,opt) -> opt = Optional) args then [], args
          else args, [] in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args) in
        let handle = protect "func" lam
        and l = List.map (fun (arg, opt) -> may_map (protect "arg") arg, opt) l
        and id_arg = Ident.create "param" in
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction(Curried, ids, lam, locid) ->
              Lfunction(Curried, id_arg::ids, lam,
                        Memprof.internal loc.l loc.p "Translcore.build_apply(1)")
          | Levent(Lfunction(Curried, ids, lam, _), _) ->
              Lfunction(Curried, id_arg::ids, lam,
                        Memprof.internal loc.l loc.p "Translcore.build_apply(2)")
          | lam ->
              Lfunction(Curried, [id_arg], lam,
                        Memprof.internal loc.l loc.p "Translcore.build_apply(3)")
        in
        List.fold_left
          (fun body (id, lam) -> Llet(Strict, id, loc, lam, body))
          body !defs
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  build_apply lam []
    (List.map (fun (l, x,o) -> may_map (transl_exp loc) x, o) sargs)

and transl_function loc untuplify_fn repr partial cases =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function(_, pl,partial')} as exp}]
      when Parmatch.fluid pat ->
        let loc = newl loc exp.exp_loc in
      let param = name_pattern "param" cases in
      let ((_, params), body) =
        transl_function loc false repr partial' pl in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body] partial)
  | {c_lhs={pat_desc = Tpat_tuple pl}} :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let params = List.map (fun p -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases loc pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        let param = name_pattern "param" cases in
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases loc cases) partial)
      end
  | _ ->
      let param = name_pattern "param" cases in
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases loc cases) partial)

and transl_let loc rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          body
      | {vb_pat=pat; vb_expr=expr} :: rem ->
          let loc = field_path loc pat in
          Matching.for_let loc (transl_exp loc expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_) -> id
            | Tpat_alias ({pat_desc=Tpat_any}, id,_) -> id
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case {vb_pat=pat; vb_expr=expr} id =
        let loc = field_path loc pat in
        let lam = transl_exp loc expr in
        if not (check_recursive_lambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      let defs = List.map2 transl_case pat_expr_list idlist in
      let locid =
        if List.for_all
          (function (id, Lfunction(_, _, _, _)) -> true | _ -> false)
          defs then
          let loc = rec_field_path loc idlist in
          Memprof.recclosure_locid loc.p
            (List.map (fun { vb_pat; vb_expr } ->
              vb_pat.pat_loc, vb_pat.pat_type) pat_expr_list)
        else
          Memprof.internal loc.l loc.p "Translcore.transl_let(1)"
      in
      Lletrec(defs, body, locid)

and transl_setinstvar loc self var expr =
  Lprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
                    [self; transl_normal_path var; transl_exp loc expr])

and transl_record locid all_labels repres lbl_expr_list opt_init_expr =
  let loc = locid.loc in
  let size = Array.length all_labels in
  (* Determine if there are "enough" new fields *)
  if 3 + 2 * List.length lbl_expr_list >= size
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let lv = Array.make (Array.length all_labels) (staticfail loc) in
    let init_id = Ident.create "init" in
    begin match opt_init_expr with
      None -> ()
    | Some init_expr ->
        for i = 0 to Array.length all_labels - 1 do
          let access =
            match all_labels.(i).lbl_repres with
              Record_regular -> Pfield i
            | Record_float ->
              let locid = Memprof.locid init_expr.exp_loc locid.loc.p Predef.type_float in
              Pfloatfield (i, locid) in
          lv.(i) <- Lprim(access, [Lvar init_id])
        done
    end;
    List.iter
      (fun (_, lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp locid.loc expr)
      lbl_expr_list;
    let ll = Array.to_list lv in
    let mut =
      if List.exists (fun (_, lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
          Record_regular -> Lconst(Const_block(0, cl))
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
      with Not_constant ->
        match repres with
          Record_regular -> Lprim(Pmakeblock(0, mut, locid), ll)
        | Record_float -> Lprim(Pmakearray (Pfloatarray, locid), ll) in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr ->
        Llet(Strict, init_id, locid.loc, transl_exp locid.loc init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    (* If you change anything here, you will likely have to change
       [check_recursive_recordwith] in this file. *)
    let copy_id = Ident.create "newrecord" in
    let update_field (_, lbl, expr) cont =
      let upd =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer expr)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      Lsequence(Lprim(upd, [Lvar copy_id; transl_exp locid.loc expr]), cont) in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, copy_id, locid.loc,
             Lprim(Pduprecord (repres, size, locid),
                   [transl_exp locid.loc init_expr]),
             List.fold_right update_field lbl_expr_list (Lvar copy_id))
    end
  end

and transl_match loc e arg pat_expr_list exn_pat_expr_list partial =
  let id = name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases loc pat_expr_list
  and exn_cases = transl_cases loc exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_negative_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (loc, static_exception_id, body), id,
                 Matching.for_trywith loc (Lvar id) exn_cases),
       (static_exception_id, val_ids),
       handler)
  in
  match arg, exn_cases with
  | {exp_desc = Texp_tuple argl}, [] ->
    Matching.for_multiple_match loc (transl_list loc argl) cases partial
  | {exp_desc = Texp_tuple argl}, _ :: _ ->
    let val_ids = List.map (fun _ -> name_pattern "val" []) argl in
    let lvars = List.map (fun id -> Lvar id) val_ids in
    static_catch (transl_list loc argl) val_ids
      (Matching.for_multiple_match loc lvars cases partial)
  | arg, [] ->
    Matching.for_function loc None (transl_exp loc arg) cases partial
  | arg, _ :: _ ->
    let val_id = name_pattern "val" pat_expr_list in
    static_catch [transl_exp loc arg] [val_id]
      (Matching.for_function loc None (Lvar val_id) cases partial)


(* Wrapper for class compilation *)

(*
let transl_exp = transl_exp_wrap

let transl_let rec_flag pat_expr_list body =
  match pat_expr_list with
    [] -> body
  | (_, expr) :: _ ->
      Translobj.oo_wrap expr.exp_env false
        (transl_let rec_flag pat_expr_list) body
*)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_letrec_pat ->
      fprintf ppf
        "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      fprintf ppf
        "This kind of expression is not allowed as right-hand side of `let rec'"
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unknown_builtin_primitive prim_name ->
    fprintf ppf  "Unknown builtin primitive \"%s\"" prim_name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
