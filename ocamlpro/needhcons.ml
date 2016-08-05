(***********************************************************************)
(*                                                                     *)
(*                              TypeRex                                *)
(*                                                                     *)
(*  Copyright 2014, Fabrice Le Fessant, INRIA/OCamlPro.                *)
(*                    All rights reserved.                             *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

open Longident
open Asttypes
open Typedtree

let ppf = Format.std_formatter

let rec is_building_constr lid exp =
  match exp.exp_desc with

#if OCAML_VERSION = "4.01.0+ocp1"

  | Texp_construct (lid_loc, _, _, _ ) when lid_loc.txt = lid ->
    true

  | Texp_ifthenelse (cond, iftrue, Some iffalse) ->
    is_building_constr lid iftrue || is_building_constr lid iffalse

  | Texp_match (_, match_cases, _) ->
    List.exists (fun (pat, exp) -> is_building_constr lid exp) match_cases

  | Texp_letmodule (_, _, _, body)
  | Texp_let (_, _, body) -> is_building_constr lid body

  | Texp_try (body, exn_cases) ->
    is_building_constr lid body ||
      List.exists (fun (pat, exp) -> is_building_constr lid exp) exn_cases


  | Texp_assertfalse
  | Texp_when (_, _) -> false
#else
  | Texp_let (_, _, _)
  | Texp_match (_, _, _, _)
  | Texp_try (_, _)
  | Texp_letmodule (_, _, _, _) -> assert false (* TODO *)
#endif
  | Texp_sequence (_, body) -> is_building_constr lid body

  | Texp_construct _ -> false (* other lid case *)

  | Texp_ifthenelse (_, _, _) (* no-else case *)
  | Texp_ident (_, _, _)
  | Texp_constant _
  | Texp_function (_, _, _)
  | Texp_apply (_, _)
  | Texp_tuple _
  | Texp_variant (_, _)
  | Texp_record (_, _)
  | Texp_field (_, _, _)
  | Texp_setfield (_, _, _, _)
  | Texp_array _
  | Texp_while (_, _)
  | Texp_for (_, _, _, _, _, _)
  | Texp_send (_, _, _)
  | Texp_new (_, _, _)
  | Texp_instvar (_, _, _)
  | Texp_setinstvar (_, _, _, _)
  | Texp_override (_, _)
  | Texp_assert _
  | Texp_lazy _
  | Texp_object (_, _)
  | Texp_pack _ -> false


let not_too_simple lid =
  match lid.txt with
  | Lident "::" -> false
  | _ -> true

let is_rebuild =
  function
  | { pat_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
      Tpat_construct (lid, _, _ :: _, _) },
#else
      Tpat_construct (lid, _, _ :: _) },
#endif
    exp when not_too_simple lid ->
    is_building_constr lid.txt exp
  | _ -> false

let rec find_rebuild match_cases =
  match match_cases with
  | [] -> false
  | case :: match_cases ->
    is_rebuild
#if OCAML_VERSION = "4.01.0+ocp1"
      case
#else
      (case.c_lhs, case.c_rhs)
#endif
    || find_rebuild match_cases

module M = TypedtreeIter.MakeIterator(struct
    include TypedtreeIter.DefaultIteratorArgument

    let enter_expression e =
      match e.exp_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
      | Texp_match (exp, match_cases, _) ->
#else
      | Texp_match (exp, match_cases, exn_cases,_) ->
#endif

        if
          Ctype.equal e.exp_env false
            [Ctype.correct_levels e.exp_type]
            [Ctype.correct_levels exp.exp_type]
          && find_rebuild match_cases
        then
          begin
            Printtyp.mark_loops e.exp_type;
            Format.fprintf ppf "%a@.Possible need for hconsing %a.@."
              Location.print_loc e.exp_loc
              Printtyp.type_expr e.exp_type
          end

      | _ -> ()
  end)

let registered = ref false
let init () =
  if not !registered then begin
    registered := true;
    try
      ignore (Sys.getenv "OCP_CHECKHCONS");
      Globalize.add_internal_implementation_rewriter "90_needhcons"
        (fun sourcefile s ->
          M.iter_structure s;
          s
        )
    with Not_found -> ()
  end

let _ = init ()
