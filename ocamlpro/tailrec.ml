(***********************************************************************)
(*                                                                     *)
(*                              TypeRex                                *)
(*                                                                     *)
(*  Copyright 2014, Pierre Chambart, OCamlPro.                         *)
(*                    All rights reserved.                             *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

(* This is a copy of tools/ocp-tailrec/tailrec.ml *)

module StringSet = Set.Make(String)

type env =
  { env : StringSet.t;
    tail : bool }

let empty_env = { env = StringSet.empty; tail = false }

#if OCAML_VERSION = "4.01.0+ocp1"

open Asttypes
open Parsetree


let rec pat_ids acc { ppat_desc } = match ppat_desc with
  | Ppat_var { txt } ->
    StringSet.add txt acc

  | Ppat_alias (pat, { txt }) ->
    pat_ids (StringSet.add txt acc) pat

  | Ppat_tuple pats
  | Ppat_array pats ->
    List.fold_left pat_ids acc pats

  | Ppat_record (pats, _) ->
    List.fold_left pat_ids acc (List.map snd pats)

  | Ppat_or (pat1, pat2) ->
    pat_ids (pat_ids acc pat1) pat2

  | Ppat_variant (_, Some pat)
  | Ppat_construct (_, Some pat, _)
  | Ppat_constraint (pat, _)
  | Ppat_lazy pat -> pat_ids acc pat

  | Ppat_variant (_, None)
  | Ppat_construct (_, None, _)
  | Ppat_constant _
  | Ppat_type _
  | Ppat_unpack _
  | Ppat_any -> acc

let warn str loc =
  Format.eprintf "%a@.Warning: %S is not tail-recursive@."
    Location.print_loc loc str

let warn_var str loc =
  Format.printf "%a@.Warning: %S is passed as a recursive argument@."
    Location.print_loc loc str

let rec structure_item env { pstr_desc; pstr_loc } =
  match pstr_desc with
  | Pstr_value (rec_flag, defs) ->
    let pats, exprs = List.split defs in
    let ids = match rec_flag with
      | Recursive -> List.fold_left pat_ids env.env pats
      | _ -> env.env in
    List.iter (expression { env with env = ids }) exprs

  | Pstr_eval expr ->
    expression env expr

  | Pstr_modtype _
  | Pstr_open _
  | Pstr_class_type _
  | Pstr_include _
  | Pstr_exn_rebind _
  | Pstr_primitive _
  | Pstr_type _
  | Pstr_exception _ -> ()

  | Pstr_module (_, modul) -> module_expr env modul

  | Pstr_recmodule modules ->
    List.iter (fun (_,_,modul) -> module_expr env modul) modules

  | Pstr_class _ ->
    Format.eprintf
      "%a@.Warning: classes are not yet supported by check-tailrec@."
      Location.print_loc pstr_loc

  (* | Pstr_class of class_declaration list *)

and structure env str =
  List.iter (structure_item env) str

and module_expr env { pmod_desc } =
  let env = { env with tail = false } in
  match pmod_desc with
  | Pmod_ident _ -> ()
  | Pmod_structure str -> structure { env with tail = false } str
  | Pmod_constraint (modul,_)
  | Pmod_functor (_,_,modul) -> module_expr env modul
  | Pmod_apply (m1,m2) ->
    module_expr env m1;
    module_expr env m2
  | Pmod_unpack expr ->
    expression env expr

and expression_not_tail env expr =
  expression { env = env; tail = false } expr

and apply_expr env ({ pexp_desc; pexp_loc } as expr) =
  match pexp_desc with
  | Pexp_ident { txt = (Longident.Lident str); loc } ->
    if not env.tail then
      if StringSet.mem str env.env then
        warn str loc
  | Pexp_ident _ ->
    (* not a simple ident: not a recursive function *)
    ()
  | _ -> expression_not_tail env.env expr

and expression env { pexp_desc; pexp_loc } =
  (* let expression' ids expr = expression { env with env = ids } expr in *)
  match pexp_desc with
  | Pexp_apply (func, params) ->
    apply_expr env func;
    (* TODO: special case of seqor and seqand: the last parameter is tail *)
    List.iter (expression_not_tail env.env) (List.map snd params)

  | Pexp_ident { txt = (Longident.Lident str); loc } ->
    if not env.tail then
      if StringSet.mem str env.env then
        warn_var str loc

  | Pexp_new _
  | Pexp_constant _
  | Pexp_ident _
  | Pexp_assertfalse -> ()

  | Pexp_function (_, default, defs) ->
    Misc.may (expression env) default;
    List.iter (expression { env with tail = true }) (List.map snd defs)

  | Pexp_let (rec_flag, defs, body) ->
    let pats, exprs = List.split defs in
    let ids_defs, env_body = match rec_flag with
      | Recursive -> List.fold_left pat_ids env.env pats, env
      | _ ->
        env.env,
        let pat_ids = List.fold_left pat_ids StringSet.empty pats in
        { env with env = StringSet.diff env.env pat_ids } in
    expression env_body body;
    List.iter (expression_not_tail ids_defs) exprs

  | Pexp_match (case, branch) ->
    expression_not_tail env.env case;
    let f (pat, expr) =
      let ids = pat_ids StringSet.empty pat in
      let env = { env with env = StringSet.diff env.env ids } in
      expression env expr
    in
    List.iter f branch

  | Pexp_array exprs
  | Pexp_tuple exprs ->
    List.iter (expression_not_tail env.env) exprs

  | Pexp_variant (_, expr)
  | Pexp_construct (_, expr, _) ->
    Misc.may (expression_not_tail env.env) expr

  | Pexp_newtype (_, expr)
  | Pexp_poly (expr, _)
  | Pexp_constraint (expr, _, _)
  | Pexp_open (_,_,expr) ->
    expression env expr

  | Pexp_when (e1,e2)
  | Pexp_sequence (e1,e2) ->
    expression_not_tail env.env e1;
    expression env e2

  | Pexp_ifthenelse (e1,e2,e3) ->
    expression_not_tail env.env e1;
    expression env e2;
    Misc.may (expression env) e3

  | Pexp_try (body, handlers) ->
    expression_not_tail env.env body;
    List.iter (expression env) (List.map snd handlers)

  | Pexp_setinstvar (_, expr)
  | Pexp_field (expr, _)
  | Pexp_send (expr, _)
  | Pexp_assert expr ->
    expression_not_tail env.env expr

  | Pexp_for (_,e1,e2,_,e3) ->
    expression_not_tail env.env e1;
    expression_not_tail env.env e2;
    expression_not_tail env.env e3

  | Pexp_setfield (e1,_,e2)
  | Pexp_while (e1,e2) ->
    expression_not_tail env.env e1;
    expression_not_tail env.env e2

  | Pexp_record (fields,with_) ->
    List.iter (expression_not_tail env.env) (List.map snd fields);
    Misc.may (expression_not_tail env.env) with_

  | Pexp_lazy expr ->
    expression {env with tail = true} expr

  | Pexp_override fields ->
    List.iter (expression env) (List.map snd fields)

  | Pexp_letmodule (_, modul, expr) ->
    module_expr env modul;
    expression env expr

  | Pexp_pack modul ->
    module_expr env modul

  | _ ->
    Format.printf "not done expr %a@." Location.print_loc pexp_loc;
    assert false

  (* | Pexp_object of class_structure *)

#else

let structure env str = assert false

#endif
