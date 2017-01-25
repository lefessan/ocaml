
open Path
open Types
open Typedtree

(** Module utile à Memprof *)

(* Dans un premier temps, on calcule une substitution (à la
   [Subst.t]) permettant de remplacer les [Ident.t] des constructeurs
   de type par des chemins absolus ayant comme racine l'unité de
   compilation.

   Les types définis à l'intérieur d'un foncteur [Make(Arg : S)] ont
   pour préfixe [Unit.Make(??Arg)].

   Les types définis dans des modules anonymes ont pour préfixe:
   'Unit.Anonymous_%l_%c', où %l et %c sont respectivement la ligne et
   la colonne du 'struct' ou du 'functor'. *)

(* Dans un second temps, on calcule la 'signature concrète' de l'unité
   de compilation, celle qui permet d'associer à un 'chemin absolu de
   types' sa définition concrète. Il sera utile pour l'algorithme de
   propagation de type 'ocp-memprof'. *)

type prefix = {
  anchored: bool;
  prefix: Path.t;
  functor_args_prefix: Path.t;
  anonymous_prefix: Path.t;
}

let enter_module env name =
  assert (env.anchored);
  { env with
    prefix = Pdot (env.prefix, name, nopos);
    functor_args_prefix = Pdot (env.prefix, "??" ^ name, nopos);
    anchored = true;
  }

let anonymous_name loc =
  let position = loc.Location.loc_start in
  Printf.sprintf "Anonymous_%d_%d"
    position.Lexing.pos_lnum
    (position.Lexing.pos_cnum - position.Lexing.pos_bol)

let anchor_env env loc =
  if env.anchored then
    env
  else
    let name = anonymous_name loc in
    let prefix = Pdot (env.anonymous_prefix, name, nopos) in
    let anonymous_prefix = prefix in
    { env with prefix; anonymous_prefix; anchored = true; }

let enter_structure env loc =
  if env.anchored then
    { env with anonymous_prefix = env.prefix }
  else
    anchor_env env loc

let enter_functor env loc arg =
  let env = anchor_env env loc in
  let prefix =
    Papply (env.prefix, Pdot (env.functor_args_prefix , arg, nopos)) in
  { env with prefix }

let rec globalize_structure env subst str =
  List.fold_left (globalize_structure_item env) subst str.str_items

and globalize_structure_item env subst item =
  match item.str_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
  | Tstr_type decls ->
      List.fold_left
        (fun subst (id, _, _) ->
           Subst.add_type id (Pdot (env.prefix, Ident.name id, nopos)) subst)
        subst decls
  | Tstr_module (id, _, mexpr) ->
      let env = enter_module env (Ident.name id) in
      let subst = Subst.add_module id env.prefix subst in
      globalize_module_expr env subst mexpr
  | Tstr_include (mexpr, _) ->
      globalize_module_expr { env with anchored = false } subst mexpr
  | Tstr_exn_rebind _ -> subst
  | Tstr_recmodule mexprs ->
      List.fold_left
        (fun subst (id, _, _, mexpr) ->
           let env = enter_module env (Ident.name id) in
           let subst = Subst.add_module id env.prefix subst in
           globalize_module_expr env subst mexpr)
        subst mexprs
#else
#if OCAML_VERSION < "4.04"
  | Tstr_type decls ->

#else
  | Tstr_type (_rec, decls) ->
#endif
    List.fold_left
        (fun subst { typ_id } ->
           Subst.add_type typ_id (Pdot (env.prefix, Ident.name typ_id, nopos)) subst)
        subst decls
  | Tstr_module { mb_id; mb_expr } ->
      let env = enter_module env (Ident.name mb_id) in
      let subst = Subst.add_module mb_id env.prefix subst in
      globalize_module_expr env subst mb_expr
  | Tstr_include { incl_mod } ->
      globalize_module_expr { env with anchored = false } subst incl_mod
  | Tstr_recmodule mexprs ->
      List.fold_left
        (fun subst { mb_id; mb_expr } ->
           let env = enter_module env (Ident.name mb_id) in
           let subst = Subst.add_module mb_id env.prefix subst in
           globalize_module_expr env subst mb_expr)
        subst mexprs
  | Tstr_attribute _
  | Tstr_typext _ -> subst
#endif
  | Tstr_class_type cls ->
      List.fold_right
        (fun (_, _, cl) subst ->
           let id = cl.ci_id_class_type in
           Subst.add_type id (Pdot(env.prefix, Ident.name id, nopos)) subst)
        cls subst
  | Tstr_class cls ->
      List.fold_right
#if OCAML_VERSION < "4.04"
        (fun (cl, _, _) subst ->
#else
        (fun (cl, _) subst ->
#endif
          let id = cl.ci_id_class_type in
           Subst.add_type id (Pdot(env.prefix, Ident.name id, nopos)) subst)
        cls subst
  | Tstr_eval _ | Tstr_value _ ->
      subst (* TODO local module; first-class module ??? *)
  | Tstr_modtype _ | Tstr_exception _
  | Tstr_open _ | Tstr_primitive _ -> subst

and globalize_module_expr env subst mexpr =
  match mexpr.mod_desc with
  | Tmod_structure str ->
      let env = enter_structure env mexpr.mod_loc in
      globalize_structure env subst str
  | Tmod_constraint (mexpr, _, _, _) ->
      globalize_module_expr env subst mexpr
  | Tmod_functor (id, _sloc, _mty, mexp) ->
      let env = enter_functor env mexpr.mod_loc (Ident.name id) in
      globalize_module_expr env subst mexp
  | Tmod_apply (m1, m2, _) ->
      let env = { env with anchored = false } in
      let subst = globalize_module_expr env subst m1 in
      globalize_module_expr env subst m2
  | Tmod_ident _ | Tmod_unpack _ -> subst


let rec mem_type name = function
  | [] -> false
  | Sig_type (id, _, _) :: _ when Ident.name id = name -> true
  | _ :: msigs -> mem_type name msigs
let rec mem_class_type name = function
  | [] -> false
  | Sig_class_type (id, _, _) :: _ when Ident.name id = name -> true
  | _ :: msigs -> mem_class_type name msigs
let rec find_module name = function
  | [] -> None
  | Sig_module (id, mty, _) :: _ when Ident.name id = name -> Some mty
  | _ :: msigs -> find_module name msigs

#if OCAML_VERSION = "4.01.0+ocp1"

#else

    let mk_md md_loc md_type =
      let md_attributes = [] in
      { md_loc; md_type; md_attributes }

#endif

let rec filter_sigs items msigs =
  (* remove from [items] the item that do not appears in msigs *)
  let filter item acc =
    match item with
    | Sig_type (id, _, _) when mem_type (Ident.name id) msigs ->
        item :: acc
    | Sig_class_type (id, _, _) when mem_class_type (Ident.name id) msigs ->
        item :: acc
#if OCAML_VERSION = "4.01.0+ocp1"
    | Sig_module (id, Mty_signature items, rs) -> begin
        match find_module (Ident.name id) msigs with
        | Some (Mty_signature msigs) ->
            let msigs = filter_sigs items msigs in
            Sig_module (id, Mty_signature msigs, rs) :: acc
        | Some _ -> item :: acc (* TODO warning ?? *)
        | None -> acc
      end
#else
    | Sig_module (id,
                  ({ md_type = Mty_signature items } as md), rs) -> begin
        match find_module (Ident.name id) msigs with
        | Some { md_type = Mty_signature msigs } ->
            let msigs = filter_sigs items msigs in
            Sig_module (id, { md with md_type = Mty_signature msigs }, rs) :: acc
        | Some _ -> item :: acc (* TODO warning ?? *)
        | None -> acc
      end
#endif
    | _ -> acc

  in
  List.fold_right filter items []

let concrete name str =

  let id = Ident.create_persistent name in

  let initial_env = {
    anchored = true;
    prefix = Pident id;
    anonymous_prefix = Pident id;
    functor_args_prefix = Pident (Ident.create ":unused:");
  } in

  let subst = globalize_structure initial_env Subst.identity str in

  let anonymous_modules = ref [[]] in
  let push_anonymous_mods () =
    anonymous_modules := [] :: !anonymous_modules in
  let pop_anonymous_mods () =
    match !anonymous_modules with
    | [] -> assert false
    | hd :: tl -> anonymous_modules := tl; hd in
  let record_anonymous loc sigs =
    match !anonymous_modules with
    | [] -> assert false
    | hd :: tl ->
        let id = Ident.create (anonymous_name loc) in
        anonymous_modules := (Sig_module (id, sigs, Trec_not) :: hd) :: tl;
        id in

  let rec concrete_structure str =
    List.fold_right concrete_structure_item str.str_items []

  and concrete_structure_item item acc =
    match item.str_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Tstr_type [(id, _, decl)] ->
        Sig_type (id, decl.typ_type, Trec_not) :: acc
    | Tstr_type ((id, _, decl) :: decls) ->
        Sig_type (id, decl.typ_type, Trec_first) ::
        List.fold_right
          (fun (id, _, decl) acc ->
             Sig_type (id, decl.typ_type, Trec_next) :: acc)
          decls acc
    | Tstr_module (id, _, mexpr) ->
        let msig = concrete_module_expr true mexpr in
        Sig_module (id, msig, Trec_not) :: acc
    | Tstr_recmodule [id, _, _, mexpr] ->
        Sig_module (id, concrete_module_expr true mexpr, Trec_first) :: acc
    | Tstr_recmodule ((id, _, _, mexpr) :: mexprs) ->
        Sig_module (id, concrete_module_expr true mexpr, Trec_first) ::
        List.fold_right
          (fun (id, _, _, mexpr) acc ->
             Sig_module (id, concrete_module_expr true mexpr, Trec_next) ::
             acc)
          mexprs acc
    | Tstr_include (mexpr, msig) -> begin
        match concrete_module_expr false mexpr with
        | Mty_signature items ->
            filter_sigs msig items @ acc
        | _ -> assert false
        end
    | Tstr_modtype (_, _, _)
    | Tstr_exn_rebind _ -> acc
#else
#if OCAML_VERSION < "4.04"
    | Tstr_type types ->
#else
    | Tstr_type (_rec, types) ->
#endif
      begin match types with
        [{ typ_id; typ_type; }] ->
          Sig_type (typ_id, typ_type, Trec_not) :: acc
      | { typ_id; typ_type } :: decls ->
        Sig_type (typ_id, typ_type, Trec_first) ::
          List.fold_right
          (fun {typ_id; typ_type} acc ->
            Sig_type (typ_id, typ_type, Trec_next) :: acc)
          decls acc
      | [] -> assert false
        end
    | Tstr_recmodule [{ mb_id; mb_expr; mb_loc }] ->
        Sig_module (mb_id, mk_md mb_loc (concrete_module_expr true mb_expr), Trec_first) :: acc
    | Tstr_recmodule ({ (* mb_id; *) mb_expr; mb_loc } :: mexprs) ->
        Sig_module (id, mk_md mb_loc (concrete_module_expr true mb_expr), Trec_first) ::
        List.fold_right
          (fun { (* mb_id; *) mb_expr; mb_loc } acc ->
             Sig_module (id, mk_md mb_loc (concrete_module_expr true mb_expr), Trec_next) ::
             acc)
          mexprs acc
    | Tstr_module { (* mb_id; *) mb_expr; mb_loc } ->
        let msig = concrete_module_expr true mb_expr in
        Sig_module (id, mk_md mb_loc msig, Trec_not) :: acc
    | Tstr_include { incl_mod; incl_type } -> begin
        match concrete_module_expr false incl_mod with
        | Mty_signature items ->
            filter_sigs incl_type items @ acc
        | _ -> assert false
        end

    | Tstr_modtype _ -> acc
    | Tstr_typext _
    | Tstr_attribute _ -> acc
#endif
    | Tstr_class_type [(id, _, cl)] ->
        Sig_class_type (id, cl.ci_type_decl, Trec_not) :: acc
    | Tstr_class_type ((id, _, cl) :: cls) ->
        Sig_class_type (id, cl.ci_type_decl, Trec_first) ::
        List.fold_right
          (fun (id, _, cl) acc ->
             Sig_class_type (id, cl.ci_type_decl, Trec_next) :: acc)
          cls acc
#if OCAML_VERSION < "4.04"
    | Tstr_class [(cl, _, _)] ->
#else
    | Tstr_class [(cl, _)] ->
#endif
      Sig_class_type (cl.ci_id_class_type, cl.ci_type_decl, Trec_not) ::
        acc
#if OCAML_VERSION < "4.04"
    | Tstr_class ((cl, _, _) :: cls) ->
#else
    | Tstr_class ((cl, _) :: cls) ->
#endif
      Sig_class_type (cl.ci_id_class_type, cl.ci_type_decl, Trec_first) ::
        List.fold_right
#if OCAML_VERSION < "4.04"
          (fun (cl, _, _) acc ->
#else
          (fun (cl, _) acc ->
#endif
      Sig_class_type (id, cl.ci_type_decl, Trec_next) :: acc)
          cls acc
    | Tstr_eval _ | Tstr_value _ ->
        acc (* TODO local module; first-class module ?? *)
    | Tstr_exception _
    | Tstr_open _ | Tstr_primitive _ -> acc
    | Tstr_recmodule []
    | Tstr_class_type [] | Tstr_class [] -> assert false

  and concrete_module_expr anchored mexpr =
    match mexpr.mod_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Tmod_structure str ->
        push_anonymous_mods ();
        let sigs = concrete_structure str in
        let msig = Mty_signature (sigs @ pop_anonymous_mods ()) in
        if anchored then
          msig
        else begin
          let id = record_anonymous mexpr.mod_loc msig in
          concrete_strengthen Env.initial (Pident id) msig
        end
    | Tmod_functor (arg, _, mty, mexpr) ->
        (* record_functor_args arg mty; *)
        Mty_functor (arg, mty.mty_type, concrete_module_expr true mexpr)
#else
    | Tmod_structure str ->
        push_anonymous_mods ();
        let sigs = concrete_structure str in
        let msig = mk_md mexpr.mod_loc (
          Mty_signature (sigs @ pop_anonymous_mods ())) in
        if anchored then
          msig.md_type
        else begin
          let id = record_anonymous mexpr.mod_loc msig in
          concrete_strengthen Env.initial_safe_string (Pident id) msig.md_type
        end

    | Tmod_functor (arg, _, Some mty, mexpr) ->
        (* record_functor_args arg mty; *)
      Mty_functor (arg, Some mty.mty_type, concrete_module_expr true mexpr)
        (* TODO: check with case with Gregoire. Generative functors
          [module F() = (val x)] might have a different way of being globalized.
        *)
    | Tmod_functor (arg, _, None, mexpr) ->
      Mty_functor (arg, None, concrete_module_expr true mexpr)
#endif
    | Tmod_ident (id, _) ->
        concrete_strengthen mexpr.mod_env id mexpr.mod_type
    | Tmod_apply (mfun, marg, _) -> begin
        match concrete_module_expr false mfun with
        | Mty_functor (param, mty_param, mty_res) -> begin
            match Typemod.path_of_module marg with
            | Some path ->
                Subst.modtype
                  (Subst.add_module param path Subst.identity)
                  mty_res
            | None ->
#if OCAML_VERSION = "4.01.0+ocp1"
#else
    if mty_param = None then mty_res else
#endif
                let marg = concrete_module_expr false marg in
                begin try
                        Mtype.nondep_supertype
                          (Env.add_module param marg mexpr.mod_env) param mty_res
                  with Not_found ->
                    Printf.eprintf "Warning: could not globalize Mtype.nondep_supertype\n%!";
                    Mty_signature [] (* TODO *)
                      end
          end
        | _ ->
          (* TODO Warning... *)
          Mty_signature [] (* Should (almost!) never happen! *)
      end
    | Tmod_constraint (mexpr, _, _, _) -> concrete_module_expr anchored mexpr
    | Tmod_unpack (_, mty) -> Mtype.scrape mexpr.mod_env mty

  and concrete_strengthen env prefix (mty : Types.module_type) =
#if OCAML_VERSION = "4.01.0+ocp1"
    match Mtype.scrape env mty with
#else
    match Env.scrape_alias env mty with
    | Mty_alias _ -> Mty_signature [] (* TODO *)
#endif
    | Mty_signature sg ->
        Mty_signature
          (List.fold_right (concrete_strengthen_item env prefix) sg [])
    | Mty_functor(param, arg, res) when !Clflags.applicative_functors ->
        Mty_functor(param, arg,
                    concrete_strengthen env (Papply(prefix, Pident param)) res)
    | Mty_ident _
    | Mty_functor (_, _, _) -> Mty_signature []


  and concrete_strengthen_item env prefix item acc =
    match item with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Sig_exception _ -> acc
    | Sig_module (id, mty, rec_status) ->
        let prefix = Pdot(prefix, Ident.name id, nopos) in
        Sig_module (id, concrete_strengthen env prefix mty, rec_status) ::
        acc
#else
    | Sig_typext (_, _, _) -> acc
    | Sig_module (id, mty, rec_status) ->
        let prefix = Pdot(prefix, Ident.name id, nopos) in
        Sig_module (id,
                    { mty with
                      md_type = concrete_strengthen env prefix mty.md_type},
                    rec_status) ::
        acc
#endif
    | Sig_type (id, decl, rec_status) ->
        let path = Pdot(prefix, Ident.name id, nopos) in
        let ty = Btype.newgenty (Tconstr(path, decl.type_params, ref Mnil)) in
        Sig_type (id, { decl with type_kind = Type_abstract;
                                  type_manifest = Some ty }, rec_status) ::
        acc
    | Sig_value _ | Sig_modtype _
    | Sig_class _ | Sig_class_type _ -> acc

  in

  (id, subst, Mty_signature (concrete_structure str))
