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

open Parsetree

let find_let_ = ref false
let find_ignore = ref false

let ppf = Format.std_formatter

module M = ParsetreeIter.MakeIterator(struct
  include ParsetreeIter.DefaultIteratorArgument

  let enter_binding pat exp =
    match pat.ppat_desc with
    | Ppat_any ->
      if !find_let_ then
        Format.fprintf ppf
          "%a@.Warning: type-escaping bindings should be avoided (let _ = ...)@."
          Location.print_loc pat.ppat_loc
    | _ -> ()

  let enter_expression exp =
    match exp.pexp_desc with
    | Pexp_apply (
      { pexp_desc = Pexp_ident { txt = Lident "ignore" } },
      ["", arg ]) ->
      begin match arg.pexp_desc with
      | Pexp_constraint _ -> () (* ignore ( exp : typ ); OK *)
      | _ ->
        if !find_ignore then
          Format.fprintf ppf
            "%a@.Warning: type-escaping bindings should be avoided (ignore (...))@."
            Location.print_loc exp.pexp_loc
      end
    | _ -> ()

  end)

let registered = ref false
let init () =
  if not !registered then begin
    registered := true;
    try
      begin match Misc.split (Sys.getenv "OCP_CHECK_STYLE1") ':' with
      | [ "0" ] -> raise Not_found
      | [ "1" ] | [] -> find_let_ := true; find_ignore := true
      | list ->
        List.iter (fun pat ->
          match pat with
          | "let_" -> find_let_ := true
          | "ignore" -> find_ignore := true
          | _ ->
            Printf.eprintf
              "Warning: OCP_CHECK_STYLE1 with %S is not understood\n%!"
              pat
        ) list
      end;
      Ocpp.add_internal_implementation_rewriter "80_style_check1"
        (fun sourcefile s ->
          M.iter_structure s;
          s)
    with Not_found -> ()
  end

let _ = init ()
