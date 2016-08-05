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

(* The "lambda" intermediate code *)

open Asttypes

type alloc = Memprof.alloc =
  | NoAlloc
  | LocId of int

type delayed_alloc = Memprof.delayed_alloc
type location = Memprof.location = { l:Location.t; p: Path.t }
type locid = Memprof.locid = { loc: location; id:delayed_alloc ref }
val newl : location -> Location.t -> location
val newp : location -> Path.t -> location
val lp : Location.t -> Path.t -> location
val unitlp : string -> location
(* val prof : locid -> alloc *)

type compile_time_constant =
  | Big_endian
  | Word_size
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS

type 'a raw_primitive =
    Pidentity
  | Pignore
 (* Memprof: Prevapply and Pdirapply may allocate partial functions *)
  | Prevapply of location
  | Pdirapply of location
  | Ploc of loc_kind
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * 'a
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int * 'a
  | Psetfloatfield of int
  | Pduprecord of Types.record_representation * int * 'a
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description * 'a option
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint of 'a
  | Pnegfloat of 'a | Pabsfloat of 'a
  | Paddfloat of 'a | Psubfloat of 'a
  | Pmulfloat of 'a | Pdivfloat of 'a
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind * 'a
  | Parraylength of array_kind
  | Parrayrefu of array_kind * 'a (* assert (LocId only for floats ) *)
  | Parraysetu of array_kind
  | Parrayrefs of array_kind * 'a
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of (boxed_integer * 'a)
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer * 'a (*destination*)
  | Pnegbint of (boxed_integer * 'a)
  | Paddbint of (boxed_integer * 'a)
  | Psubbint of (boxed_integer * 'a)
  | Pmulbint of (boxed_integer * 'a)
  | Pdivbint of (boxed_integer * 'a)
  | Pmodbint of (boxed_integer * 'a)
  | Pandbint of (boxed_integer * 'a)
  | Porbint of (boxed_integer * 'a)
  | Pxorbint of (boxed_integer * 'a)
  | Plslbint of (boxed_integer * 'a)
  | Plsrbint of (boxed_integer * 'a)
  | Pasrbint of (boxed_integer * 'a)
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout * location
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool * location
  | Pstring_load_64 of bool * location
  | Pstring_set_16 of bool
  | Pstring_set_32 of bool
  | Pstring_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool * location
  | Pbigstring_load_64 of bool * location
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of (boxed_integer * 'a)
  (* Integer to external pointer *)
  | Pint_as_pointer

and primitive = locid raw_primitive

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable
(* Meaning of kinds for let x = e in e':
    Strict: e may have side-effets; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
    Variable: the variable x is assigned later in e' *)

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list     (* stack size -> code label *)

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list * location
  (* Memprof: Lfunction may allocate a closure *)
  | Lfunction of function_kind * Ident.t list * lambda * locid
  | Llet of let_kind * Ident.t * location * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda * locid
  | Lprim of primitive * lambda list
  | Lswitch of lambda * lambda_switch
(* switch on strings, clauses are sorted by string order,
   strings are pairwise distinct *)
  | Lstringswitch of lambda * (string * lambda) list * lambda option * location
  | Lstaticraise of location * int * lambda list
  | Lstaticcatch of lambda * (int * Ident.t list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * location
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_failaction : lambda option}      (* Action to take if failure *)
and lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.summary }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function

(* Sharing key *)
val make_key: lambda -> lambda option

val const_unit: structured_constant
val lambda_unit: lambda
val name_lambda: location -> let_kind -> lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list: location -> lambda list -> (lambda list -> lambda) -> lambda

val iter: (lambda -> unit) -> lambda -> unit
module IdentSet: Set.S with type elt = Ident.t
val free_variables: lambda -> IdentSet.t
val free_methods: lambda -> IdentSet.t

val transl_normal_path: Path.t -> lambda   (* Path.t is already normal *)
val transl_path: ?loc:Location.t -> Env.t -> Path.t -> lambda
val make_sequence: ('a -> lambda) -> 'a list -> lambda

val subst_lambda: lambda Ident.tbl -> lambda -> lambda
val bind : location -> let_kind -> Ident.t -> lambda -> lambda -> lambda

val commute_comparison : comparison -> comparison
val negate_comparison : comparison -> comparison

(***********************)
(* For static failures *)
(***********************)

(* Get a new static failure ident *)
val next_raise_count : unit -> int
val next_negative_raise_count : unit -> int
  (* Negative raise counts are used to compile 'match ... with
     exception x -> ...'.  This disabled some simplifications
     performed by the Simplif module that assume that static raises
     are in tail position in their handler. *)

val staticfail : location -> lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded: lambda -> bool
val patch_guarded : lambda -> lambda -> lambda

val lam_of_loc : loc_kind -> Location.t -> lambda

val raise_kind: raise_kind -> string
val lam_of_loc : loc_kind -> Location.t -> lambda

val reset: unit -> unit
