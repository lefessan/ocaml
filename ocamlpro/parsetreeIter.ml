(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Fabrice Le Fessant (INRIA Saclay)                                   *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

module type IteratorArgument = sig

#if OCAML_VERSION = "4.01.0+ocp1"
   val enter_exception_declaration : exception_declaration -> unit
   val leave_exception_declaration : exception_declaration -> unit
   val enter_modtype_declaration : modtype_declaration -> unit
   val leave_modtype_declaration : modtype_declaration -> unit
   val enter_core_field_type : core_field_type -> unit
   val leave_core_field_type : core_field_type -> unit
#else
  val enter_value_binding : value_binding -> unit
  val leave_value_binding : value_binding -> unit
#endif
    val enter_structure : structure -> unit
    val enter_value_description : value_description -> unit
    val enter_type_declaration : type_declaration -> unit
    val enter_pattern : pattern -> unit
    val enter_expression : expression -> unit
    val enter_package_type : package_type -> unit
    val enter_signature : signature -> unit
    val enter_signature_item : signature_item -> unit
    val enter_module_type : module_type -> unit
    val enter_module_expr : module_expr -> unit
    val enter_with_constraint : with_constraint -> unit
    val enter_class_expr : class_expr -> unit
    val enter_class_signature : class_signature -> unit
    val enter_class_declaration : class_declaration -> unit
    val enter_class_description : class_description -> unit
    val enter_class_type_declaration : class_type_declaration -> unit
    val enter_class_type : class_type -> unit
    val enter_class_type_field : class_type_field -> unit
    val enter_core_type : core_type -> unit
    val enter_class_structure : class_structure -> unit
    val enter_class_field : class_field -> unit
    val enter_structure_item : structure_item -> unit


    val leave_structure : structure -> unit
    val leave_value_description : value_description -> unit
    val leave_type_declaration : type_declaration -> unit
    val leave_pattern : pattern -> unit
    val leave_expression : expression -> unit
    val leave_package_type : package_type -> unit
    val leave_signature : signature -> unit
    val leave_signature_item : signature_item -> unit
    val leave_module_type : module_type -> unit
    val leave_module_expr : module_expr -> unit
    val leave_with_constraint : with_constraint -> unit
    val leave_class_expr : class_expr -> unit
    val leave_class_signature : class_signature -> unit
    val leave_class_declaration : class_declaration -> unit
    val leave_class_description : class_description -> unit
    val leave_class_type_declaration : class_type_declaration -> unit
    val leave_class_type : class_type -> unit
    val leave_class_type_field : class_type_field -> unit
    val leave_core_type : core_type -> unit
    val leave_class_structure : class_structure -> unit
    val leave_class_field : class_field -> unit
    val leave_structure_item : structure_item -> unit

    val enter_bindings : rec_flag -> unit
    val enter_binding : pattern -> expression -> unit
    val leave_binding : pattern -> expression -> unit
    val leave_bindings : rec_flag -> unit

      end

module MakeIterator(Iter : IteratorArgument) : sig

    val iter_structure : structure -> unit
    val iter_signature : signature -> unit
    val iter_structure_item : structure_item -> unit
    val iter_signature_item : signature_item -> unit
    val iter_expression : expression -> unit
    val iter_module_type : module_type -> unit
    val iter_pattern : pattern -> unit
    val iter_class_expr : class_expr -> unit

  end = struct

    let may_iter f v =
      match v with
        None -> ()
      | Some x -> f x

#if OCAML_VERSION = "4.01.0+ocp1"
    open Asttypes
#endif

    let rec iter_structure str =
      Iter.enter_structure str;
      List.iter iter_structure_item str;
      Iter.leave_structure str


    and iter_structure_item item =
      Iter.enter_structure_item item;
      begin
        match item.pstr_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pstr_eval exp -> iter_expression exp
        | Pstr_primitive (_, v) -> iter_value_description v
        | Pstr_exception (_, decl) -> iter_exception_declaration decl
        | Pstr_exn_rebind (p, _) -> ()
        | Pstr_module (_, mexpr) -> iter_module_expr mexpr
        | Pstr_modtype (_, mtype) -> iter_module_type mtype
        | Pstr_value (rec_flag, list) -> iter_bindings rec_flag list
        | Pstr_type list ->
          List.iter (fun (_, decl) -> iter_type_declaration decl) list
        | Pstr_recmodule list ->
          List.iter (fun (_, mtype, mexpr) ->
            iter_module_type mtype;
            iter_module_expr mexpr) list
        | Pstr_include mexpr ->
          iter_module_expr mexpr

#else
        | Pstr_eval (_, _)|Pstr_value (_, _)|Pstr_primitive _|Pstr_type _
        | Pstr_typext _|Pstr_exception _|Pstr_module _|Pstr_recmodule _
        |Pstr_modtype _|Pstr_attribute _|Pstr_extension (_, _) -> assert false
        | Pstr_include _ -> assert false
#endif
        | Pstr_open _ -> ()
        | Pstr_class list ->
          List.iter (fun ci ->
            Iter.enter_class_declaration ci;
            iter_class_expr ci.pci_expr;
            Iter.leave_class_declaration ci;
          ) list
        | Pstr_class_type list ->
          List.iter (fun ct ->
            Iter.enter_class_type_declaration ct;
            iter_class_type ct.pci_expr;
            Iter.leave_class_type_declaration ct;
          ) list
      end;
      Iter.leave_structure_item item

    and iter_value_description v =
      Iter.enter_value_description v;
      iter_core_type v.pval_type;
      Iter.leave_value_description v

    and iter_type_declaration decl =
      Iter.enter_type_declaration decl;
      List.iter (fun (ct1, ct2, loc) ->
        iter_core_type ct1;
        iter_core_type ct2
      ) decl.ptype_cstrs;
      begin match decl.ptype_kind with
        Ptype_abstract -> ()
#if OCAML_VERSION = "4.01.0+ocp1"
      | Ptype_variant list ->
        List.iter (fun (s, cts, cto, loc) ->
          List.iter iter_core_type cts;
          may_iter iter_core_type cto
        ) list
      | Ptype_record list ->
        List.iter (fun (s, mut, ct, loc) ->
          iter_core_type ct
        ) list
#else
      |Ptype_variant _|Ptype_record _|Ptype_open -> assert false
#endif
      end;
      begin match decl.ptype_manifest with
        None -> ()
      | Some ct -> iter_core_type ct
      end;
      Iter.leave_type_declaration decl

    and iter_pattern pat =
      Iter.enter_pattern pat;
      begin
        match pat.ppat_desc with
          Ppat_any -> ()
        | Ppat_var (_) -> ()
        | Ppat_type _ -> ()
        | Ppat_unpack _ -> ()
        | Ppat_alias (pat1, _) -> iter_pattern pat1
        | Ppat_constant cst -> ()
        | Ppat_tuple list ->
          List.iter iter_pattern list
#if OCAML_VERSION = "4.01.0+ocp1"
        | Ppat_construct (_, args, _) ->  may_iter iter_pattern args
        | Ppat_constraint (pat, ct) -> iter_pattern pat; iter_core_type ct
#else
        | Ppat_interval (_, _)|Ppat_construct (_, _)|Ppat_constraint (_, _)
        | Ppat_exception _|Ppat_extension _ -> assert false
#endif
        | Ppat_variant (label, pato) ->
          begin match pato with
            None -> ()
          | Some pat -> iter_pattern pat
          end
        | Ppat_record (list, closed) ->
          List.iter (fun (_, pat) -> iter_pattern pat) list
        | Ppat_array list -> List.iter iter_pattern list
        | Ppat_or (p1, p2) -> iter_pattern p1; iter_pattern p2
        | Ppat_lazy p -> iter_pattern p
      end;
      Iter.leave_pattern pat

    and iter_expression exp =
      Iter.enter_expression exp;
      begin
        match exp.pexp_desc with
          Pexp_ident path -> ()
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pexp_constraint (e, cty1, cty2) ->
          iter_expression e;
          may_iter iter_core_type cty1; may_iter iter_core_type cty2
        | Pexp_function (label, expo, cases) ->
          may_iter iter_expression expo;
          iter_bindings Nonrecursive cases
        | Pexp_construct (_, expo, _) ->
          may_iter iter_expression expo
        | Pexp_when (exp1, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_assertfalse -> ()
        | Pexp_let (rec_flag, list, exp) ->
          iter_bindings rec_flag list;
          iter_expression exp
        | Pexp_match (exp, list) ->
          iter_expression exp;
          iter_bindings Nonrecursive list
        | Pexp_try (exp, list) ->
          iter_expression exp;
          iter_bindings Nonrecursive list
        | Pexp_letmodule (_, mexpr, exp) ->
          iter_module_expr mexpr;
          iter_expression exp
        | Pexp_pack (mexpr) ->
          iter_module_expr mexpr

#else
        | Pexp_let (_, _, _)|Pexp_function _|Pexp_fun (_, _, _, _)
        |Pexp_match (_, _)|Pexp_try (_, _)|Pexp_construct (_, _)
        |Pexp_constraint (_, _)|Pexp_coerce (_, _, _)
        |Pexp_letmodule (_, _, _)|Pexp_pack _|Pexp_extension _ -> assert false
#endif
        | Pexp_constant cst -> ()
        | Pexp_open (_, path, _) -> ()
        | Pexp_poly (e, cto) ->
          iter_expression e;
          may_iter iter_core_type cto
        | Pexp_newtype (s, e) -> iter_expression e
        | Pexp_apply (exp, list) ->
          iter_expression exp;
          List.iter (fun (label, exp) -> iter_expression exp) list
        | Pexp_tuple list ->
          List.iter iter_expression list
        | Pexp_variant (label, expo) ->
          may_iter iter_expression expo
        | Pexp_record (list, expo) ->
          List.iter (fun (_, exp) -> iter_expression exp) list;
          may_iter iter_expression expo
        | Pexp_field (exp, label) ->
          iter_expression exp
        | Pexp_setfield (exp1, label, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_array list ->
          List.iter iter_expression list
        | Pexp_ifthenelse (exp1, exp2, expo) ->
          iter_expression exp1;
          iter_expression exp2;
          begin match expo with
            None -> ()
          | Some exp -> iter_expression exp
          end
        | Pexp_sequence (exp1, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_while (exp1, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_for (_, exp1, exp2, dir, exp3) ->
          iter_expression exp1;
          iter_expression exp2;
          iter_expression exp3
        | Pexp_send (exp, meth) ->
          iter_expression exp;
        | Pexp_new path -> ()
        | Pexp_setinstvar (_, exp) ->
          iter_expression exp
        | Pexp_override list ->
          List.iter (fun (path, exp) ->
            iter_expression exp
          ) list
        | Pexp_assert exp -> iter_expression exp
        | Pexp_lazy exp -> iter_expression exp
        | Pexp_object cl ->
          iter_class_structure cl
      end;
      Iter.leave_expression exp;

    and iter_package_type pack =
      Iter.enter_package_type pack;
      List.iter (fun (s, ct) -> iter_core_type ct) (snd pack);
      Iter.leave_package_type pack;

    and iter_signature sg =
      Iter.enter_signature sg;
      List.iter iter_signature_item sg;
      Iter.leave_signature sg;

    and iter_signature_item item =
      Iter.enter_signature_item item;
      begin
        match item.psig_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
        | Psig_value (_, v) -> iter_value_description v
        | Psig_exception (_, decl) -> iter_exception_declaration decl
        | Psig_module (_, mtype) -> iter_module_type mtype
        | Psig_modtype (_, mdecl) -> iter_modtype_declaration mdecl
        | Psig_type list ->
          List.iter (fun (_, decl) ->
            iter_type_declaration decl
          ) list
        | Psig_recmodule list ->
          List.iter (fun (_, mtype) -> iter_module_type mtype) list
        | Psig_include mty -> iter_module_type mty
#else
        | Psig_value _|Psig_type _|Psig_typext _|Psig_exception _
        |Psig_module _|Psig_recmodule _|Psig_modtype _
        |Psig_attribute _|Psig_extension (_, _) -> assert false
        | Psig_include mty -> assert false
#endif
        | Psig_open _ -> ()
        | Psig_class list ->
          List.iter iter_class_description list
        | Psig_class_type list ->
          List.iter iter_class_type_declaration list
      end;
      Iter.leave_signature_item item;


    and iter_class_description cd =
      Iter.enter_class_description cd;
      iter_class_type cd.pci_expr;
      Iter.leave_class_description cd;

    and iter_class_type_declaration cd =
      Iter.enter_class_type_declaration cd;
      iter_class_type cd.pci_expr;
      Iter.leave_class_type_declaration cd;

    and iter_module_type mty =
      Iter.enter_module_type mty;
      begin
        match mty.pmty_desc with
          Pmty_ident lid -> ()
        | Pmty_signature sg -> iter_signature sg
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pmty_functor (_, mtype1, mtype2) ->
          iter_module_type mtype1; iter_module_type mtype2
        | Pmty_with (mtype, list) ->
          iter_module_type mtype;
          List.iter (fun (path, withc) ->
            iter_with_constraint withc
          ) list
        | Pmty_typeof mexpr ->
          iter_module_expr mexpr
#else
        | Pmty_functor (_, _, _)|Pmty_with (_, _)|Pmty_typeof _
        |Pmty_extension _|Pmty_alias _ -> assert false
#endif
      end;
      Iter.leave_module_type mty;

    and iter_with_constraint cstr =
      Iter.enter_with_constraint cstr;
      begin
        match cstr with
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pwith_type decl -> iter_type_declaration decl
#else
        | Pwith_type _ -> assert false
#endif
        | Pwith_module _ -> ()
        | Pwith_typesubst decl -> iter_type_declaration decl
        | Pwith_modsubst _ -> ()
      end;
      Iter.leave_with_constraint cstr;

    and iter_module_expr mexpr =
      Iter.enter_module_expr mexpr;
      begin
        match mexpr.pmod_desc with
          Pmod_ident p -> ()
        | Pmod_structure st -> iter_structure st
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pmod_functor (_, mtype, mexpr) ->
          iter_module_type mtype;
          iter_module_expr mexpr
#else
        | Pmod_functor (_, _, _)|Pmod_extension _ -> assert false
#endif
        | Pmod_apply (mexp1, mexp2) ->
          iter_module_expr mexp1;
          iter_module_expr mexp2
        | Pmod_constraint (mexpr, mod_type ) ->
          iter_module_expr mexpr;
          iter_module_type mod_type
        | Pmod_unpack exp ->
          iter_expression exp
      (*          iter_module_type mty *)
      end;
      Iter.leave_module_expr mexpr;

    and iter_class_expr cexpr =
      Iter.enter_class_expr cexpr;
      begin
        match cexpr.pcl_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
#else
        | Pcl_extension _ -> assert false
#endif
        | Pcl_constraint (cl, ct) ->
          iter_class_expr cl;
          iter_class_type ct
        | Pcl_structure clstr -> iter_class_structure clstr
        | Pcl_fun (label, expo, pat, cl) ->
          may_iter iter_expression expo;
          iter_pattern pat;
          iter_class_expr cl

        | Pcl_apply (cl, args) ->
          iter_class_expr cl;
          List.iter (fun (label, exp) ->
            iter_expression exp
          ) args

        | Pcl_let (rec_flat, bindings, cl) ->
          iter_bindings rec_flat bindings;
          iter_class_expr cl

        | Pcl_constr (_, tyl) ->
          List.iter iter_core_type tyl
      end;
      Iter.leave_class_expr cexpr;

    and iter_class_type ct =
      Iter.enter_class_type ct;
      begin
        match ct.pcty_desc with
          Pcty_signature csg -> iter_class_signature csg
        | Pcty_constr (path, list) ->
          List.iter iter_core_type list
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pcty_fun (label, ct, cl) ->
          iter_core_type ct;
          iter_class_type cl
#else
        |Pcty_arrow (_, _, _)|Pcty_extension _ -> assert false
#endif
      end;
      Iter.leave_class_type ct;

    and iter_class_signature cs =
      Iter.enter_class_signature cs;
      iter_core_type cs.pcsig_self;
      List.iter iter_class_type_field cs.pcsig_fields;
      Iter.leave_class_signature cs


    and iter_class_type_field ctf =
      Iter.enter_class_type_field ctf;
      begin
        match ctf.pctf_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pctf_inher ct -> iter_class_type ct
        | Pctf_virt  (s, priv, ct) -> iter_core_type ct
        | Pctf_meth  (s, priv, ct) -> iter_core_type ct
        | Pctf_cstr  (ct1, ct2) ->
          iter_core_type ct1;
          iter_core_type ct2
#else
        | Pctf_inherit _|Pctf_method _|Pctf_constraint _|Pctf_attribute _
        | Pctf_extension _ -> assert false
#endif
        | Pctf_val (s, mut, virt, ct) ->
          iter_core_type ct
      end;
      Iter.leave_class_type_field ctf

    and iter_core_type ct =
      Iter.enter_core_type ct;
      begin
        match ct.ptyp_desc with
          Ptyp_any -> ()
#if OCAML_VERSION = "4.01.0+ocp1"
        | Ptyp_object list ->
          List.iter iter_core_field_type list
        | Ptyp_class (path, list, labels) ->
          List.iter iter_core_type list
#else
        | Ptyp_object (_, _)|Ptyp_class (_, _)|Ptyp_extension _ -> assert false
#endif
        | Ptyp_var s -> ()
        | Ptyp_arrow (label, ct1, ct2) ->
          iter_core_type ct1;
          iter_core_type ct2
        | Ptyp_tuple list -> List.iter iter_core_type list
        | Ptyp_constr (path, list) ->
          List.iter iter_core_type list
        | Ptyp_alias (ct, s) ->
          iter_core_type ct
        | Ptyp_variant (list, bool, labels) ->
          List.iter iter_row_field list
        | Ptyp_poly (list, ct) -> iter_core_type ct
        | Ptyp_package pack -> iter_package_type pack
      end;
      Iter.leave_core_type ct;

    and iter_class_structure cs =
      Iter.enter_class_structure cs;
#if OCAML_VERSION = "4.01.0+ocp1"
      iter_pattern cs.pcstr_pat;
#else
      iter_pattern cs.pcstr_self;
#endif
      List.iter iter_class_field cs.pcstr_fields;
      Iter.leave_class_structure cs;


    and iter_row_field rf =
      match rf with
#if OCAML_VERSION = "4.01.0+ocp1"
      | Rtag (label, bool, list) ->
          List.iter iter_core_type list
#else
      | Rtag (_, _, _, _) -> assert false
#endif
      | Rinherit ct -> iter_core_type ct

    and iter_class_field cf =
      Iter.enter_class_field cf;
      begin
        match cf.pcf_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
        | Pcf_inher (ovf, cl, super) -> iter_class_expr cl
        | Pcf_constr (cty, cty') ->
          iter_core_type cty;
          iter_core_type cty'
        | Pcf_valvirt (name, mut, cty) -> iter_core_type cty
        | Pcf_val (name, mut, override, exp) -> iter_expression exp
        | Pcf_virt (name, mut, cty) -> iter_core_type cty
        | Pcf_meth (name, priv, override, exp) -> iter_expression exp
        | Pcf_init exp -> iter_expression exp
#else
        | Pcf_inherit (ovf, cl, super) -> iter_class_expr cl
        | Pcf_val _|Pcf_method _|Pcf_constraint _|Pcf_initializer _
        |Pcf_attribute _|Pcf_extension _ -> assert false
#endif
      end;
      Iter.leave_class_field cf;

    and iter_binding (pat, exp) =
      Iter.enter_binding pat exp;
      iter_pattern pat;
      iter_expression exp;
      Iter.leave_binding pat exp

    and iter_bindings rec_flag list =
      Iter.enter_bindings rec_flag;
#if OCAML_VERSION = "4.01.0+ocp1"
      List.iter iter_binding list;
#else
      List.iter iter_value_binding list;
#endif
      Iter.leave_bindings rec_flag

#if OCAML_VERSION = "4.01.0+ocp1"
    and iter_exception_declaration decl =
      Iter.enter_exception_declaration decl;
      List.iter iter_core_type decl;
      Iter.leave_exception_declaration decl;

    and iter_modtype_declaration mdecl =
      Iter.enter_modtype_declaration mdecl;
      begin
        match mdecl with
          Pmodtype_abstract -> ()
        | Pmodtype_manifest mtype -> iter_module_type mtype
      end;
      Iter.leave_modtype_declaration mdecl;

    and iter_core_field_type cft =
      Iter.enter_core_field_type cft;
      begin match cft.pfield_desc with
        Pfield_var -> ()
      | Pfield (s, ct) -> iter_core_type ct
      end;
      Iter.leave_core_field_type cft;
#else

  and iter_value_binding v =
    Iter.enter_value_binding v;
    iter_pattern v.pvb_pat;
    iter_expression v.pvb_expr;
    Iter.leave_value_binding v

#endif

  end

module DefaultIteratorArgument = struct

#if OCAML_VERSION = "4.01.0+ocp1"
      let enter_exception_declaration _ = ()
      let leave_exception_declaration _ = ()
      let enter_modtype_declaration _ = ()
      let leave_modtype_declaration _ = ()
      let enter_core_field_type _ = ()
      let leave_core_field_type _ = ()
#else
      let enter_value_binding _ = ()
      let leave_value_binding _ = ()
#endif

      let enter_structure _ = ()
      let enter_value_description _ = ()
      let enter_type_declaration _ = ()
      let enter_pattern _ = ()
      let enter_expression _ = ()
      let enter_package_type _ = ()
      let enter_signature _ = ()
      let enter_signature_item _ = ()
      let enter_module_type _ = ()
      let enter_module_expr _ = ()
      let enter_with_constraint _ = ()
      let enter_class_expr _ = ()
      let enter_class_signature _ = ()
      let enter_class_declaration _ = ()
      let enter_class_description _ = ()
      let enter_class_type_declaration _ = ()
      let enter_class_type _ = ()
      let enter_class_type_field _ = ()
      let enter_core_type _ = ()
      let enter_class_structure _ = ()
    let enter_class_field _ = ()
    let enter_structure_item _ = ()


      let leave_structure _ = ()
      let leave_value_description _ = ()
      let leave_type_declaration _ = ()
      let leave_pattern _ = ()
      let leave_expression _ = ()
      let leave_package_type _ = ()
      let leave_signature _ = ()
      let leave_signature_item _ = ()
      let leave_module_type _ = ()
      let leave_module_expr _ = ()
      let leave_with_constraint _ = ()
      let leave_class_expr _ = ()
      let leave_class_signature _ = ()
      let leave_class_declaration _ = ()
      let leave_class_description _ = ()
      let leave_class_type_declaration _ = ()
      let leave_class_type _ = ()
      let leave_class_type_field _ = ()
      let leave_core_type _ = ()
      let leave_class_structure _ = ()
    let leave_class_field _ = ()
    let leave_structure_item _ = ()

    let enter_binding _ _ = ()
    let leave_binding _ _ = ()

    let enter_bindings _ = ()
    let leave_bindings _ = ()

  end
