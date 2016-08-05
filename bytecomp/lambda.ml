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

open Misc
open Path
open Asttypes

(* Instead of using an integer for locids, we use a full record. This
   way, we are able to extract the location and path information,
   reducing the amount of parameters to pass to all functions. Also,
   the locid is always a lazy, to delay its computation and reduce the
   number of allocated locids *)
type alloc = Memprof.alloc =
  | NoAlloc
  | LocId of int

type delayed_alloc = Memprof.delayed_alloc
type location = Memprof.location = { l:Location.t; p: Path.t }
type locid = Memprof.locid = { loc: location; id:delayed_alloc ref }
let newl lp loc = { lp with l = loc }
let newp lp path = { lp with p = path }
let lp l p = { l;p }
let unitlp unitname = { l = Location.none;
                    p = Pident (Ident.create_persistent unitname) }
(* let prof locid = Lazy.force locid.id *)

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

(* The ['a] becomes [locid] in the lambda-code [primitive] type.
    Fabrice: I don't know what other type it can take.

   [[visit:../../ocamlpro/docs/devel/lambda.ml:primitive]]
  *)
type 'a raw_primitive =
    Pidentity
  | Pignore
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
  | Parrayrefu of array_kind * 'a
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

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list * location
  | Lfunction of function_kind * Ident.t list * lambda * locid
  | Llet of let_kind * Ident.t * location * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda * locid
  | Lprim of primitive * lambda list
  | Lswitch of lambda * lambda_switch
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
  { sw_numconsts: int;
    sw_consts: (int * lambda) list;
    sw_numblocks: int;
    sw_blocks: (int * lambda) list;
    sw_failaction : lambda option}

and lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.summary }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function

let const_unit = Const_pointer 0

let lambda_unit = Lconst const_unit

(* Build sharing keys *)
(*
   Those keys are later compared with Pervasives.compare.
   For that reason, they should not include cycles.
*)

exception Not_simple

let max_raw = 32

let make_key e =
  let count = ref 0   (* Used for controling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count ;
    if !count > max_raw then raise Not_simple ; (* Too big ! *)
    match e with
    | Lvar id ->
      begin
        try Ident.find_same id env
        with Not_found -> e
      end
    | Lconst  (Const_base (Const_string _)|Const_float_array _) ->
        (* Mutable constants are not shared *)
        raise Not_simple
    | Lconst _ -> e
    | Lapply (e,es,loc) ->
        Lapply (tr_rec env e,tr_recs env es,loc)
    | Llet (Alias,x,_loc, ex,e) -> (* Ignore aliases -> substitute *)
        let ex = tr_rec env ex in
        tr_rec (Ident.add x ex env) e
    | Llet (str,x,loc, ex,e) ->
     (* Because of side effects, keep other lets with normalized names *)
        let ex = tr_rec env ex in
        let y = make_key x in
        Llet (str,y,loc,ex,tr_rec (Ident.add x (Lvar y) env) e)
    | Lprim (p,es) ->
        Lprim (p,tr_recs env es)
    | Lswitch (e,sw) ->
        Lswitch (tr_rec env e,tr_sw env sw)
    | Lstringswitch (e,sw,d, locid) ->
        Lstringswitch
          (tr_rec env e,
           List.map (fun (s,e) -> s,tr_rec env e) sw,
           tr_opt env d, locid)
    | Lstaticraise (loc, i,es) ->
        Lstaticraise (loc, i,tr_recs env es)
    | Lstaticcatch (e1,xs,e2) ->
        Lstaticcatch (tr_rec env e1,xs,tr_rec env e2)
    | Ltrywith (e1,x,e2) ->
        Ltrywith (tr_rec env e1,x,tr_rec env e2)
    | Lifthenelse (cond,ifso,ifnot) ->
        Lifthenelse (tr_rec env cond,tr_rec env ifso,tr_rec env ifnot)
    | Lsequence (e1,e2) ->
        Lsequence (tr_rec env e1,tr_rec env e2)
    | Lassign (x,e) ->
        Lassign (x,tr_rec env e)
    | Lsend (m,e1,e2,es,loc) ->
        Lsend (m,tr_rec env e1,tr_rec env e2,tr_recs env es,loc)
    | Lifused (id,e) -> Lifused (id,tr_rec env e)
    | Lletrec _|Lfunction _
    | Lfor _ | Lwhile _
(* Beware: (PR#6412) the event argument to Levent
   may include cyclic structure of type Type.typexpr *)
    | Levent _  ->
        raise Not_simple

  and tr_recs env es = List.map (tr_rec env) es

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_consts ;
      sw_blocks = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_blocks ;
      sw_failaction = tr_opt env sw.sw_failaction ; }

  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e) in

  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda loc strict arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.create "let" in Llet(strict, id, loc, arg, fn id)

let name_lambda_list loc args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar id as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create "let" in
      Llet(Strict, id, loc, arg, name_list (Lvar id :: names) rem) in
  name_list [] args


let iter_opt f = function
  | None -> ()
  | Some e -> f e

let iter f = function
    Lvar _
  | Lconst _ -> ()
  | Lapply(fn, args, _) ->
      f fn; List.iter f args
  | Lfunction(kind, params, body, _) ->
      f body
  | Llet(str, id, _, arg, body) ->
      f arg; f body
  | Lletrec(decl, body, _) ->
      f body;
      List.iter (fun (id, exp) -> f exp) decl
  | Lprim(p, args) ->
      List.iter f args
  | Lswitch(arg, sw) ->
      f arg;
      List.iter (fun (key, case) -> f case) sw.sw_consts;
      List.iter (fun (key, case) -> f case) sw.sw_blocks;
      iter_opt f sw.sw_failaction
  | Lstringswitch (arg,cases,default,_) ->
      f arg ;
      List.iter (fun (_,act) -> f act) cases ;
      iter_opt f default
  | Lstaticraise (_, _,args) ->
      List.iter f args
  | Lstaticcatch(e1, (_,vars), e2) ->
      f e1; f e2
  | Ltrywith(e1, exn, e2) ->
      f e1; f e2
  | Lifthenelse(e1, e2, e3) ->
      f e1; f e2; f e3
  | Lsequence(e1, e2) ->
      f e1; f e2
  | Lwhile(e1, e2) ->
      f e1; f e2
  | Lfor(v, e1, e2, dir, e3) ->
      f e1; f e2; f e3
  | Lassign(id, e) ->
      f e
  | Lsend (k, met, obj, args, _) ->
      List.iter f (met::obj::args)
  | Levent (lam, evt) ->
      f lam
  | Lifused (v, e) ->
      f e


module IdentSet =
  Set.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let free_ids get l =
  let fv = ref IdentSet.empty in
  let rec free l =
    iter free l;
    fv := List.fold_right IdentSet.add (get l) !fv;
    match l with
      Lfunction(kind, params, body, _) ->
        List.iter (fun param -> fv := IdentSet.remove param !fv) params
    | Llet(str, id, _, arg, body) ->
        fv := IdentSet.remove id !fv
    | Lletrec(decl, body, _) ->
        List.iter (fun (id, exp) -> fv := IdentSet.remove id !fv) decl
    | Lstaticcatch(e1, (_,vars), e2) ->
        List.iter (fun id -> fv := IdentSet.remove id !fv) vars
    | Ltrywith(e1, exn, e2) ->
        fv := IdentSet.remove exn !fv
    | Lfor(v, e1, e2, dir, e3) ->
        fv := IdentSet.remove v !fv
    | Lassign(id, e) ->
        fv := IdentSet.add id !fv
    | Lvar _ | Lconst _ | Lapply _
    | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
    | Lifthenelse _ | Lsequence _ | Lwhile _
    | Lsend _ | Levent _ | Lifused _ -> ()
  in free l; !fv

let free_variables l =
  free_ids (function Lvar id -> [id] | _ -> []) l

let free_methods l =
  free_ids (function Lsend(Self, Lvar meth, obj, _, _) -> [meth] | _ -> []) l

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

let negative_raise_count = ref 0

let next_negative_raise_count () =
  decr negative_raise_count ;
  !negative_raise_count

(* Anticipated staticraise, for guards *)
let staticfail loc = Lstaticraise (loc, 0,[])

let rec is_guarded = function
  | Lifthenelse( cond, body, Lstaticraise (_, 0,[])) -> true
  | Llet(str, id, _, lam, body) -> is_guarded body
  | Levent(lam, ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (_, 0,[])) ->
      Lifthenelse (cond, body, patch)
  | Llet(str, id, loc, lam, body) ->
      Llet (str, id, loc, lam, patch_guarded patch body)
  | Levent(lam, ev) ->
      Levent (patch_guarded patch lam, ev)
  | _ -> fatal_error "Lambda.patch_guarded"

(* Translate an access path *)

let rec transl_normal_path = function
    Pident id ->
      if Ident.global id then Lprim(Pgetglobal id, []) else Lvar id
  | Pdot(p, s, pos) ->
      Lprim(Pfield pos, [transl_normal_path p])
  | Papply(p1, p2) ->
      fatal_error "Lambda.transl_path"

(* Translation of value identifiers *)

let transl_path ?(loc=Location.none) env path =
  transl_normal_path (Env.normalize_path (Some loc) env path)

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in Lsequence(lam, make_sequence fn rem)

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst = function
    Lvar id as l ->
      begin try Ident.find_same id s with Not_found -> l end
  | Lconst sc as l -> l
  | Lapply(fn, args, locid) ->
    Lapply(subst fn, List.map subst args, locid)
  | Lfunction(kind, params, body, locid) ->
      Lfunction(kind, params, subst body, locid)
  | Llet(str, id, loc, arg, body) -> Llet(str, id, loc, subst arg, subst body)
  | Lletrec(decl, body, locid) ->
      Lletrec(List.map subst_decl decl, subst body, locid)
  | Lprim(p, args) -> Lprim(p, List.map subst args)
  | Lswitch(arg, sw) ->
      Lswitch(subst arg,
              {sw with sw_consts = List.map subst_case sw.sw_consts;
                       sw_blocks = List.map subst_case sw.sw_blocks;
                       sw_failaction = subst_opt  sw.sw_failaction; })
  | Lstringswitch (arg,cases,default,loc) ->
      Lstringswitch
        (subst arg,List.map subst_strcase cases,subst_opt default,loc)
  | Lstaticraise (loc, i,args) ->  Lstaticraise (loc, i, List.map subst args)
  | Lstaticcatch(e1, io, e2) -> Lstaticcatch(subst e1, io, subst e2)
  | Ltrywith(e1, exn, e2) -> Ltrywith(subst e1, exn, subst e2)
  | Lifthenelse(e1, e2, e3) -> Lifthenelse(subst e1, subst e2, subst e3)
  | Lsequence(e1, e2) -> Lsequence(subst e1, subst e2)
  | Lwhile(e1, e2) -> Lwhile(subst e1, subst e2)
  | Lfor(v, e1, e2, dir, e3) -> Lfor(v, subst e1, subst e2, dir, subst e3)
  | Lassign(id, e) -> Lassign(id, subst e)
  | Lsend (k, met, obj, args, loc) ->
      Lsend (k, subst met, subst obj, List.map subst args, loc)
  | Levent (lam, evt) -> Levent (subst lam, evt)
  | Lifused (v, e) -> Lifused (v, subst e)
  and subst_decl (id, exp) = (id, subst exp)
  and subst_case (key, case) = (key, subst case)
  and subst_strcase (key, case) = (key, subst case)
  and subst_opt = function
    | None -> None
    | Some e -> Some (subst e)
  in subst lam


(* To let-bind expressions to variables *)

let bind loc str var exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, var, loc, exp, body)

and commute_comparison = function
| Ceq -> Ceq| Cneq -> Cneq
| Clt -> Cgt | Cle -> Cge
| Cgt -> Clt | Cge -> Cle

and negate_comparison = function
| Ceq -> Cneq| Cneq -> Ceq
| Clt -> Cge | Cle -> Cgt
| Cgt -> Cle | Cge -> Clt

let lam_of_loc kind loc =
  let loc_start = loc.Location.loc_start in
  let (file, lnum, cnum) = Location.get_pos_info loc_start in
  let enum = loc.Location.loc_end.Lexing.pos_cnum -
      loc_start.Lexing.pos_cnum + cnum in
  match kind with
  | Loc_POS ->
    Lconst (Const_block (0, [
          Const_immstring file;
          Const_base (Const_int lnum);
          Const_base (Const_int cnum);
          Const_base (Const_int enum);
        ]))
  | Loc_FILE -> Lconst (Const_immstring file)
  | Loc_MODULE -> Lconst (Const_immstring
                      (String.capitalize
                         (Filename.chop_extension (Filename.basename file))))
  | Loc_LOC ->
    let loc = Printf.sprintf "File %S, line %d, characters %d-%d"
        file lnum cnum enum in
    Lconst (Const_immstring loc)
  | Loc_LINE -> Lconst (Const_base (Const_int lnum))

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "raise_notrace"

let lam_of_loc kind loc =
  let loc_start = loc.Location.loc_start in
  let (file, lnum, cnum) = Location.get_pos_info loc_start in
  let enum = loc.Location.loc_end.Lexing.pos_cnum -
      loc_start.Lexing.pos_cnum + cnum in
  match kind with
  | Loc_POS ->
    Lconst (Const_block (0, [
          Const_immstring file;
          Const_base (Const_int lnum);
          Const_base (Const_int cnum);
          Const_base (Const_int enum);
        ]))
  | Loc_FILE -> Lconst (Const_immstring file)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let module_name =
      try String.capitalize (Filename.chop_extension filename)
      with Invalid_argument _ -> "//"^filename^"//"
    in Lconst (Const_immstring module_name)
  | Loc_LOC ->
    let loc = Printf.sprintf "File %S, line %d, characters %d-%d"
        file lnum cnum enum in
    Lconst (Const_immstring loc)
  | Loc_LINE -> Lconst (Const_base (Const_int lnum))

let reset () =
  raise_count := 0
