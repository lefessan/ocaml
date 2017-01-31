(* TODO: we should verify that the patch was applied for every function not
   ignored

#option Patches "<patch-definition>"

where patch-definition is "COMMAND:ARGUMENT1:ARGUMENT2"
* "add_function_argument:ARGUMENT:FUNCTIONS" where FUNCTIONS are separated by ;
* "box_constructors:LABELS:CONSTRUCTORS"
* "add_method_arguments:ARGUMENT:METHODS"
* "replace_ident:LONGIDENT:LONGIDENTS" { Sys.command -> Sys.system }
* "add_external_flags:FLAGS:PRIMITIVES"      { caml_md5_chan -> "!memprof" }

We might also want:
* To replace a record declaration by a function:
{ x=A; y=B; z=C; }   ===>  make_record A B C

 {prim_name = "caml_greaterthan"; prim_arity = 2; prim_alloc = true;
               prim_native_name = ""; prim_native_float = false}
becomes
  prim_alloc "caml_greaterthan" 2

* To apply additional arguments on function identifiers or on
   function applications

We might also want:
* To add a patch only in a given context (structure or signature item)

Patches can also be put in a file "SOURCEFILE.ocpp".

Another idea:
A patch file "patches.ocpp" in the same directory, with the following syntax:

(* a set of patch definitions *)
begin patch "box:cmm"
  box_constructors {exp=_} [ "Cop" "..." ]
end
(* a binding between a file and a set of patches *)
begin file "cmmgen.ml"
  patch "box:cmm" "box:clambda"
end

   Actually, we could go the other way: we could modify our code, and let
   the tool infer semantic patches, and then reapply these patches on the
   new version.
*)

open Patch_types
open Asttypes
open Longident
open Parsetree
open Parsetree_map

let is_longident s =
  let len = String.length s in
  len > 0 &&
          (match s.[0] with
            'a'..'z'| 'A'..'Z' |'_' -> true
          | _ -> false)
  &&
    (try
       for i = 1 to len - 1 do
         match s.[i] with
           'a'..'z'| 'A'..'Z' | '0' ..'9' | '.' | '_' -> ()
         | _ -> raise Exit
       done;
       true
     with Exit -> false)

#if OCAML_VERSION >= "4.03"
let nolabel = Nolabel
#else
let nolabel = ""
#endif

exception Patch_failure

(* module StringSet = Set.Make(String)*)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module LongidentMap = Map.Make(struct
  type t = Longident.t
  let compare = compare
end)

module Longident = struct
  include Longident
  let to_string t = String.concat "." (Longident.flatten t)
end

let verbose = try ignore (Sys.getenv "OCPP_VERBOSE_PATCHES"); true with _ -> false

let flag_add_dbi = ref false
let box_constructors = ref StringMap.empty
let ignore_functions = ref StringMap.empty
let needy_functions = ref StringMap.empty

  (*
let map_of_list list =
  let set = ref StringMap.empty in
  List.iter (fun (s,v) ->
    set := StringMap.add s (ref false, v) !set) list;
  !set
  *)
    (*
let add_to_set set list =
  List.iter (fun s ->
    set := StringSet.add (ref false, s) !set) list
    *)

let add_to_map set list v =
  List.iter (fun s ->
    set := StringMap.add s (v, ref false) !set) list

let add_method_arguments = ref StringMap.empty
let add_function_arguments = ref StringMap.empty
let replace_ident = ref StringMap.empty
let add_external_flags = ref StringMap.empty
let add_constructor_arguments_pat = ref StringMap.empty
let add_constructor_arguments_typ = ref StringMap.empty
let add_constructor_arguments_exp = ref StringMap.empty
let add_record_fields = ref StringMap.empty

let free_vars exp =
  let set = ref StringSet.empty in
  let module Iterator = Parsetree_iter.MakeIterator(struct
    open Parsetree_iter
    include DefaultIteratorArgument

    let enter_expression exp =
      match exp.pexp_desc with
      | Pexp_ident { txt = lident } ->
        let n = Longident.to_string lident in
        set := StringSet.add n !set
      | _ -> ()
  end) in
  Iterator.iter_expression exp;
  !set

let force_loc = ref Location.none
module RelocPatch = Parsetree_map.MakeMap(struct
  include DefaultMapArgument

  let leave_expression exp =
    match exp.pexp_desc with
    | Pexp_ident { txt } ->
      { exp with
        pexp_desc = Pexp_ident { txt; loc = !force_loc };
        pexp_loc = !force_loc }
    | _ ->
      { exp with pexp_loc = !force_loc }
end)

let parse_expression_with_loc arg loc =
  Lexer.init ();
  Location.input_name := "//toplevel//";
  let lexbuf = Lexing.from_string arg in
  Location.init lexbuf "//toplevel//";
  try
    force_loc := loc;
    RelocPatch.map_expression (Parser.parse_expression Lexer.token lexbuf)
  with exn ->
    let b = Printexc.get_backtrace () in
    Printf.eprintf "Patch: Parsing error in \"%s\"\n%s\n%s\n%!" arg (Printexc.to_string exn) b;
    raise exn

module CmmgenPatch = Parsetree_map.MakeMap(struct
  include DefaultMapArgument



  let need_dbi = ref false



    (*
      let rec args_of_function loc rem exp =
      match exp.pexp_desc with
      | Pexp_function ("",None, [
      pat, body]) ->
      args_of_function loc ((pat, body) :: rem) body
      | Pexp_function ("", None, (((pat,_):: _) as pats)) ->
      args_of_function loc rem
      { exp with pexp_desc = Pexp_function("", None,[
      { pat with ppat_desc = Ppat_var {loc with txt="ocp_arg"}},
      { exp with pexp_desc = Pexp_match(
      {exp with pexp_desc =
      Pexp_ident({loc with txt=Lident"ocp_arg"})}, pats)}])}
      | _ -> rem

      let function_of_args exp bindings =
      let rec iter exp bindings =
      match bindings with
      | [] -> assert false
      | [pat, body] ->
      { exp with pexp_desc =
      Pexp_function("", None, [pat,body])}
      | (pat, body) :: args ->
      { exp with pexp_desc =
      Pexp_function("",
      None, [pat,
      iter body args])}
      in
      iter exp (List.rev bindings)
    *)

  let leave_pattern pat =
    match pat.ppat_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Ppat_construct({ txt = constr} as loc, exp, bool) ->
#else
    | Ppat_construct({ txt = constr} as loc, exp) ->
#endif
  let constr = String.concat "." (Longident.flatten constr) in
  begin try
              let (idents, used) = StringMap.find constr !box_constructors in
              used := true;
              match idents with
                [] -> assert false
              | ident :: _idents ->
              { pat with ppat_desc =
                  Ppat_record([ {loc with txt =
                      Lident ident}, pat], Closed); }
        with Not_found ->
          try
            let ((pos,args,total), used) =
              StringMap.find constr !add_constructor_arguments_pat
            in
            used := true;
            let add_args =
              List.map (fun arg ->
                  { pat with ppat_desc =
                               (if arg = "_" then Ppat_any else
                                  Ppat_var {loc with txt=arg}) }
                ) args
            in
            let add_list orig =
              match pos with
              | Start -> add_args @ orig
              | End -> orig @ add_args
              | Pos i ->
                let rec aux i before after =
                  if i = 0 then before @ add_args @ after
                  else
                    begin
                      match after with
                      | [] ->
                        Printf.eprintf
                          "Invalid patch: not enough arguments to constructor %s\n%!"
                          constr;
                        raise Patch_failure
                      | x :: tl -> aux (i-1) (before @ [x]) tl
                    end
                in
                if i < 0
                then begin
                  Printf.eprintf "Invalid patch: negative position\n%!";
                  raise Patch_failure
                end
                else aux i [] orig
            in
            match exp with
            | Some ({ ppat_desc = Ppat_tuple pat_args } as arg_pat) ->
              if total >= 0 && total <> List.length pat_args then raise Not_found;
              {
                pat with ppat_desc = Ppat_construct(loc, Some
                  { arg_pat with ppat_desc = Ppat_tuple (
                    add_list pat_args) }
#if OCAML_VERSION = "4.01.0+ocp1"
                  ,bool)
#else
                  )
#endif
      }
  | Some { ppat_desc = Ppat_any } when total <> 1 ->      pat
            | Some ({ ppat_desc =
                    ( Ppat_var _
                    | Ppat_any
                    | Ppat_record _
                    | Ppat_alias (_, _)
                    | Ppat_constant _
                    | Ppat_interval (_, _)
                    | Ppat_construct (_, _)
                    | Ppat_variant (_, _)
                    | Ppat_array _
                    | Ppat_or (_, _)
                    | Ppat_constraint (_, _)
                    | Ppat_type _
                    | Ppat_lazy _
                    | Ppat_unpack _
                    | Ppat_exception _
                    | Ppat_extension _
                    | Ppat_open (_, _)
                    )
                    } as arg_pat) ->
              if total >= 0 && total <> 1 then raise Not_found;
              {
                pat with ppat_desc = Ppat_construct(loc, Some
                  { ppat_desc = Ppat_tuple (add_list [arg_pat]);
                    ppat_loc = loc.loc;
#if OCAML_VERSION = "4.01.0+ocp1"
#else
                    ppat_attributes = [];
#endif
                  }
#if OCAML_VERSION = "4.01.0+ocp1"
                  ,bool)
#else
                  )
#endif
              }
            | None ->
              if total >= 0 && total <> 0 then raise Not_found;
              {
                pat with ppat_desc = Ppat_construct(loc, Some
                  { ppat_desc = begin
                      match args with
                      | [] -> failwith "nothing to patch"
                      | [arg] ->
                        if arg = "_" then Ppat_any else
                          Ppat_var {loc with txt=arg}
                      | _ ->
                        Ppat_tuple
                          (add_list []) end;
                    ppat_loc = loc.loc;
#if OCAML_VERSION = "4.01.0+ocp1"
#else
                    ppat_attributes = [];
#endif
                  }
#if OCAML_VERSION = "4.01.0+ocp1"
                  ,bool)
#else
                  )
#endif
              }
(*
            |  Some
                {ppat_desc=(Ppat_alias (_, _)|Ppat_constant _|Ppat_interval (_, _)|
             Ppat_construct (_, _)|Ppat_variant (_, _)|Ppat_array _|
             Ppat_or (_, _)|Ppat_constraint (_, _)|Ppat_type _|Ppat_lazy _|
             Ppat_unpack _|Ppat_exception _|Ppat_extension _|
             Ppat_open (_, _)); } ->
              Location.print Format.err_formatter loc.loc;
  Printf.kprintf failwith "cannot add argument to %S" constr *)
          with Not_found ->
          pat
      end
    | _ -> pat



  let rec mk_function pats exp =
    match pats with
    | [] -> exp
    | pat :: pats ->
      let exp = mk_function pats exp in
      let pexp_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
        Pexp_function("", None, [ pat, exp])
#else
        Pexp_fun(nolabel, None, pat, exp)
#endif
      in
      { exp with pexp_desc }


  let do_add_primitive_flags val_desc =
    let prim = List.hd val_desc.pval_prim in
    let (flags, used) = StringMap.find prim !add_external_flags in
    used := true;
    if verbose then
      Printf.eprintf "Patch: prim %S add flags %s\n%!" prim
        (String.concat " + " flags);
    { val_desc with
#if OCAML_VERSION < "4.04"
    pval_prim = val_desc.pval_prim @ flags;
#else
    pval_attributes = val_desc.pval_attributes @
                (List.map (fun attr ->
                  { val_desc.pval_name with txt = attr }, PStr[]) flags);
#endif
    }

  let do_add_function_argument really_need_dbi values =
        List.map (fun
#if OCAML_VERSION = "4.01.0+ocp1"
            (pvb_pat, pvb_expr) ->
#else
          ({ pvb_pat; pvb_expr } as pvb) ->
#endif

        let pvb_pat, pvb_expr =
          match pvb_pat, pvb_expr with
          | { ppat_desc = Ppat_var ({ txt=fname} as loc)},
#if OCAML_VERSION = "4.01.0+ocp1"
            { pexp_desc = Pexp_function _ }  ->
#else
            { pexp_desc = Pexp_function _ | Pexp_fun _ }  ->
#endif
            begin
           try
             let (args, used) = StringMap.find fname !add_function_arguments in
             used := true;
             if verbose then begin
               List.iter (fun arg ->
                 Printf.eprintf
                   "patch function definition %S: add argument %S\n%!" fname arg;
               ) args;
             end;
             let pvb_expr = mk_function
               (List.map (fun arg ->
                 { pvb_pat with ppat_desc = Ppat_var {loc with txt=arg}})
                  args)
                 pvb_expr  in
             needy_functions := StringMap.add fname ([], ref false)
               !needy_functions ;
                 (*          Printf.eprintf "need dbi: %S\n%!" fname; *)
             pvb_pat,pvb_expr

           with Not_found ->
             let really_need_dbi = really_need_dbi ||
               StringMap.mem fname !needy_functions in
             if not ( really_need_dbi && !flag_add_dbi ) then begin
               if !flag_add_dbi then
                 Printf.eprintf "leave_structure_item: %S is NOT needy %b %b\n%!" fname
                   really_need_dbi !flag_add_dbi;
               pvb_pat,pvb_expr

             end else begin

               if StringMap.mem fname !ignore_functions then
                 pvb_pat, pvb_expr
               else
                 let pvb_expr = mk_function
                   [{ pvb_pat with ppat_desc = Ppat_var {loc with txt="dbi"}}]
                     pvb_expr in
                 if verbose then
                   Printf.eprintf "leave_structure_item: %S is needy\n%!" fname;
                 needy_functions := StringMap.add fname ([], ref false)
                   !needy_functions;
                     (*          Printf.eprintf "need dbi: %S\n%!" fname; *)
                 pvb_pat,pvb_expr
             end
         end
            | _ -> pvb_pat, pvb_expr
        in
#if OCAML_VERSION = "4.01.0+ocp1"
           (pvb_pat, pvb_expr)
#else
          { pvb with pvb_pat; pvb_expr }
#endif
        ) values

  let leave_expression exp =
    match exp.pexp_desc with

    | Pexp_let (rec_flag, values, e) ->
      let values = do_add_function_argument false values in
      { exp with pexp_desc = Pexp_let (rec_flag, values, e) }

    | Pexp_record (fields, None) ->

      let new_fields = ref None in
      List.iter (fun ( { txt=field } as loc, exp) ->
        try
          let field = Longident.to_string field in
          let (fields, used) = StringMap.find field !add_record_fields in
          used := true;
          new_fields := Some (loc, exp, fields)
        with Not_found -> ()
      ) fields;
      begin match !new_fields with
        None -> exp
      | Some (loc, _field_exp, new_fields) ->
        let old_fields = ref [] in
        List.iter (fun ({txt=field},_) -> old_fields :=
          Longident.to_string field :: !old_fields) fields;
        let fields = ref fields in
        List.iter (fun field ->
          if not (List.mem field !old_fields) then begin
            if verbose then
              Printf.eprintf "Patch: add record field %S \n%!" field;
            let field = Longident.parse field in
            fields := !fields @
              [{loc with txt=field}, { exp with pexp_desc =
                  Pexp_ident {loc with txt=field}  }]
              end
        ) new_fields;
        { exp with pexp_desc = Pexp_record (!fields, None) }
      end

#if OCAML_VERSION = "4.01.0+ocp1"
    | Pexp_construct({ txt = constr} as loc, expo, bool) ->
#else
    | Pexp_construct({ txt = constr} as loc, expo) ->
#endif
      let constr = String.concat "." (Longident.flatten constr) in
      begin try
              let (idents, used) = StringMap.find constr !box_constructors in
              used := true;
              match idents with
                [] -> assert false
              | ident :: idents ->
                need_dbi := true;
                { exp with pexp_desc =
                    Pexp_record(
                      ({loc with txt = Lident ident}, exp) ::
                        List.map (fun ident ->
                          {loc with txt = Lident ident},
                          { exp with pexp_desc =
                              Pexp_ident {loc with txt = Lident ident }}
                        ) idents, None); }
        with Not_found -> try
            let ((pos,args,total), used) =
              StringMap.find constr !add_constructor_arguments_exp in
            used := true;
            let arg_to_exp arg =
              if is_longident arg then
                let lident = Longident.parse arg in
                { exp with pexp_desc = Pexp_ident { loc with txt = lident } }
              else
              parse_expression_with_loc arg loc.loc
            in
            let exps = List.map arg_to_exp args in
            Parsing.clear_parser ();
            let add_list orig =
              match pos with
              | Start -> exps @ orig
              | End -> orig @ exps
              | Pos i ->
                let rec aux i before after =
                  if i = 0 then before @ exps @ after
                  else
                    begin
                      match after with
                      | [] ->
                        Printf.eprintf
                          "Invalid patch: not enough arguments to constructor %s\n%!"
                          constr;
                        raise Patch_failure
                      | x :: tl -> aux (i-1) (before @ [x]) tl
                    end
                in
                if i < 0
                then begin
                  Printf.eprintf "Invalid patch: negative position\n%!";
                  raise Patch_failure
                end
                else aux i [] orig
            in
            match expo with
            | None ->
              if total >= 0 && total <> 0 then raise Not_found;
              begin
                match exps with
                | [] -> exp
                | [e] ->
                  {exp with pexp_desc = Pexp_construct (loc,
                     Some e
#if OCAML_VERSION = "4.01.0+ocp1"
                     , bool)
#else
                     )
#endif
                  }
                | _ ->
                  {exp with pexp_desc = Pexp_construct (loc,
                     Some { pexp_desc = Pexp_tuple(exps);
                            pexp_loc = exp.pexp_loc;
#if OCAML_VERSION = "4.01.0+ocp1"
#else
                            pexp_attributes = [];
#endif
                          }
#if OCAML_VERSION = "4.01.0+ocp1"
                     , bool)
#else
                     )
#endif
                  }
              end
            | Some ({ pexp_desc = Pexp_tuple expl } as etup) ->
              if total >= 0 && total <> List.length expl then raise Not_found;
              {exp with pexp_desc = Pexp_construct (loc,
                 Some {etup with pexp_desc = Pexp_tuple (add_list expl)}
#if OCAML_VERSION = "4.01.0+ocp1"
                     , bool)
#else
                     )
#endif
              }
            | Some (earg) ->
              if total >= 0 && total <> 1 then raise Not_found;
              {exp with pexp_desc = Pexp_construct (loc,
                 Some {earg with pexp_desc = Pexp_tuple (add_list [earg])}
#if OCAML_VERSION = "4.01.0+ocp1"
                     , bool)
#else
                     )
#endif
              }
        with Not_found -> exp
      end

    (* in 4.05.0+trunk, name is a Location.loc, maybe a need for version test *)
    | Pexp_apply({ pexp_desc = Pexp_send(_, { txt = name }) } as send_exp,
                 args) ->
      begin
        try
          let (new_args, used) = StringMap.find name !add_method_arguments in
          used := true;
          if verbose then
            Printf.eprintf
              "Patching method application %S with args %S\n%!" name
              (String.concat " " new_args);
          let new_args = List.map (fun arg ->
            nolabel,
            { exp with
              pexp_desc = Pexp_ident {txt=Lident arg;
                                      loc = exp.pexp_loc} }) new_args in
          { exp with pexp_desc = Pexp_apply(send_exp, new_args @ args) }
        with Not_found -> exp
      end


    | Pexp_apply({ pexp_desc = Pexp_ident
        ({ txt=Lident fname } as loc)} as f, args)
        when fname= "header" && !flag_add_dbi
        ->
      need_dbi := true;
          { exp with pexp_desc =
              Pexp_apply(f,
                         (nolabel,{ exp with pexp_desc =
                             Pexp_ident {loc with txt = Lident "dbi" }})::args)
          }


    (* add_function_argument... *)
    | Pexp_ident ({ txt=fname } as loc) ->
      let fname = Longident.to_string fname in
      begin
        try
          let (new_ident, used) = StringMap.find fname !replace_ident in
          let new_ident = Longident.parse new_ident in
          used := true;
          if verbose then
            Printf.eprintf "Patch: replace %S by %S\n%!" fname
              (Longident.to_string new_ident);
          { exp with pexp_desc = Pexp_ident { loc with txt = new_ident } }
        with Not_found ->
      try
        let (args, used) = StringMap.find fname !add_function_arguments in
        used := true;
        if verbose then begin
          List.iter (fun arg ->
            Printf.eprintf "patch function use %S: add argument %S\n%!" fname arg;
          ) args
        end;
        { exp with pexp_desc =
            Pexp_apply(exp,
                       List.map (fun arg ->
                         nolabel,{ exp with pexp_desc =
                             Pexp_ident {loc with txt = Longident.parse arg }}) args)
        }
      with Not_found ->
        if  !flag_add_dbi && StringMap.mem fname !needy_functions then begin
          need_dbi := true;
          { exp with pexp_desc =
              Pexp_apply(exp,
                         [nolabel,{ exp with pexp_desc =
                             Pexp_ident {loc with txt = Lident "dbi" }}])
          }
        end else
          exp
    end

  | _ -> exp


  let mk_pat ppat_desc ppat_loc =
#if OCAML_VERSION = "4.01.0+ocp1"
    { ppat_desc; ppat_loc;  }
#else
    { ppat_desc; ppat_loc; ppat_attributes = [] }
#endif

  let leave_class_field cf =
    match cf.pcf_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Pcf_meth(( {txt=name} as loc, priv, override,
#else
    | Pcf_method( {txt=name} as loc, priv, Cfk_concrete (override,
#endif
                ({ pexp_desc = Pexp_poly(exp, poly) } as poly_exp) )) ->
      begin
        try
          (*          Printf.eprintf "Patching method %S ?\n%!" name; *)
          let (new_args, used) = StringMap.find name !add_method_arguments in
          used := true;
          if verbose then
            Printf.eprintf "Patching method %S with arg %S\n%!" name
              (String.concat " " new_args);
          let exp = mk_function (List.map (fun new_arg ->
            mk_pat (Ppat_var {loc with txt=new_arg})  exp.pexp_loc)
                                   new_args) exp  in
          let exp = { poly_exp with
            pexp_desc = Pexp_poly(exp, poly) } in
          { cf with pcf_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
              Pcf_meth(loc, priv, override, exp)
#else
              Pcf_method(loc, priv, Cfk_concrete (override, exp))
#endif
          }
        with Not_found -> cf
      end
    | _ -> cf

  let mk_type ct txt =
    { ct with
      ptyp_desc = Ptyp_constr(
        {txt;loc=ct.ptyp_loc},[]);
    }

  let rec mk_function_type types ct =
    match types with
    | [] -> ct
    | ty :: types ->
      { ct with
        ptyp_desc = Ptyp_arrow(nolabel,
                               ty,
                               mk_function_type types ct) }

  let leave_class_type_field tcf = match tcf.pctf_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Pctf_meth  (name, priv,
#elif OCAML_VERSION < "4.04"
    | Pctf_method  (name, priv, virt,
#else
    | Pctf_method  ({ txt = name } as name_loc, priv, virt,
#endif
                  ({ ptyp_desc = Ptyp_poly(strings, ct)} as poly_type)) ->
      begin
        try
          let (argtypes, used) = StringMap.find name !add_method_arguments in
          used := true;
          if verbose then
            Printf.eprintf "Patching method %S with arg %S\n%!" name
              (String.concat " " argtypes);
          (*            (String.concat "." (Longident.flatten argtype)); *)
          let ct = mk_function_type (List.map (fun argtype ->
            let argtype = Longident.parse argtype in
            mk_type ct argtype) argtypes) ct in
          { tcf with pctf_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
              Pctf_meth(name, priv,
#elif OCAML_VERSION < "4.04"
              Pctf_method(name, priv, virt,
#else
              Pctf_method(name_loc, priv, virt,
#endif
                          { poly_type with
                         ptyp_desc = Ptyp_poly(strings, ct) }) }
        with Not_found -> tcf
      end
    | _ -> tcf

  let stack = ref []
  let enter_structure_item str =
    stack := !need_dbi :: !stack;
    need_dbi := false;
    str


  let leave_signature_item sg =
    match sg.psig_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Psig_value (loc, val_desc )
#else
    | Psig_value val_desc
#endif
        when val_desc.pval_prim <> [] ->
  begin try
          let newval = do_add_primitive_flags val_desc in
              { sg with psig_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
                  Psig_value(loc, newval)
#else
                  Psig_value newval
#endif
               }
        with Not_found ->
          sg
      end
#if OCAML_VERSION = "4.01.0+ocp1"
    | Psig_value ( {txt=name} as loc,({
#else
    | Psig_value (({ pval_name = {txt=name};
#endif
                   pval_prim = []; pval_type } as val_desc)) ->
      begin
        try
          let (argtypes, used) = StringMap.find name !add_function_arguments in
          used := true;
          if verbose then begin
            List.iter (fun argtype ->
              Printf.eprintf "Patch: val %S add argument %s\n%!" name argtype;
            ) argtypes
          end;
          { sg with psig_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
   Psig_value(loc,
#else
                  Psig_value(
#endif
  {
    val_desc with
      pval_type = List.fold_left (fun pval_type argtype ->
                  let argtype = Longident.parse argtype in
        { pval_type with
          ptyp_desc = Ptyp_arrow(nolabel,
                                 mk_type pval_type argtype,
                                 pval_type) }) pval_type argtypes;
   })}
        with Not_found -> sg
      end

    | _ -> sg

  let leave_structure_item str =
    let really_need_dbi = !need_dbi in
    need_dbi := (match !stack with
    | [] -> assert false
    | old_need_dbi :: old_stack ->
      stack := old_stack;
      old_need_dbi;
    );
    match str.pstr_desc with
    | Pstr_value(recflag, values) ->
      let values = do_add_function_argument really_need_dbi values in
      { str with pstr_desc = Pstr_value(recflag, values) }
#if OCAML_VERSION = "4.01.0+ocp1"
    | Pstr_primitive (loc, val_desc ) ->
#else
    | Pstr_primitive val_desc ->
#endif
      begin try
              let newval = do_add_primitive_flags val_desc in

              { str with pstr_desc =
#if OCAML_VERSION = "4.01.0+ocp1"
                  Pstr_primitive(loc,
#else
                  Pstr_primitive(
#endif
                 newval) }
        with Not_found ->
          str
      end

    | _ -> str

     let build_constructor_arguments_typ ptyp_loc args =
       List.map (fun arg ->
         let ptyp_desc =
           if is_longident arg then
             let lident = Longident.parse arg in
             Ptyp_constr ({ txt = lident; loc = ptyp_loc }, [])
           else
             let len = String.length arg in
             if len > 0 && arg.[0] = '\'' then
               Ptyp_var (String.sub arg 1 (len-1))
             else assert false
         in
         {
           ptyp_desc;
           ptyp_attributes = [];
           ptyp_loc;
         }
       ) args


     let rec do_add_constructor_arguments_typ pos args new_args =
       if pos = 0 then new_args @ args
       else
       match args with
       | [] -> failwith "bad position"
       | arg :: args ->
         arg :: do_add_constructor_arguments_typ (pos-1) args new_args

     let do_add_constructor_arguments_typ = function
#if OCAML_VERSION < "4.02"
       | ( (name, pcd_args, pcd_res, pcd_loc) as cstr )
#else
       | { pcd_args = Pcstr_record _ } as cstr -> cstr
       | { pcd_name = { txt = name }; pcd_loc;
           pcd_args = Pcstr_tuple pcd_args } as cstr
#endif
           ->
     try
       let ( (pos, new_args, total), used ) = StringMap.find name
         !add_constructor_arguments_typ in
       used := true;
       if total >= 0 && List.length pcd_args <> total then raise Not_found;
       if verbose then
         Printf.eprintf "Patching constructor %S with arg %S\n%!" name
              (String.concat " " new_args);
       let new_args = build_constructor_arguments_typ pcd_loc new_args in
       let pcd_args =
         match pos with
         | Start -> new_args @ pcd_args
         | End -> pcd_args @ new_args
         | Pos pos ->
           do_add_constructor_arguments_typ pos pcd_args new_args in
#if OCAML_VERSION < "4.02"
         ( name, pcd_args, cto, pcd_loc)
#else
         { cstr with pcd_args = Pcstr_tuple pcd_args }
#endif
     with Not_found ->
       cstr

   let leave_type_declaration decl =
     match decl.ptype_kind with
#if OCAML_VERSION >= "4.02"
      | Ptype_open
#endif
     | Ptype_abstract
     | Ptype_record _ -> decl
     | Ptype_variant list ->
       let list = List.map do_add_constructor_arguments_typ list in
       { decl with ptype_kind = Ptype_variant list }

end)

let cmm_constructors = [
  "Cop"; "Cconst_int"; "Clet"; "Cifthenelse";
  "Cvar"; "Cconst_natint"; "Cconst_symbol";
  "Cconst_pointer"; "Cconst_natpointer"; "Cconst_float";
  "Cassign";
  "Ctuple";
  "Csequence";
  "Cswitch";
  "Cloop";
  "Ccatch";
  "Cexit";
  "Ctrywith";
  "alloc_closure_header_noalloc";
]

let cmmgen_ignored =
  [
    "make_prim";
    "make_offset";
    "make_isout";
    "make_isin";
    "make_if";
    "make_switch";
    "ocpbind";
    "header";

    "transl";
    "transl_unbox_let";
    "transl_letrec";
    "transl_unbox_float";
    "transl_unbox_int";
    "transl_prim_1";
    "transl_prim_2";
    "transl_prim_3";
    "transl_switch";
    "make_catch";
    "exit_if_false";
    "exit_if_true";
    "make_catch2";

    "compunit";
    "apply_function";
    "send_function";
    "curry_function";
    "entry_point";
    "intermediate_curry_functions";
  ]

let cmmgen_needy_functions =
 [
    "unbox_float";
    "remove_unit";
    "unbox_int";
    "transl_letrec";
 ]

let clambda_constructors = [
  "Uvar";
  "Uconst";
  "Udirect_apply";
  "Ugeneric_apply";
  "Uclosure";
  "Uoffset";
  "Ulet";
  "Uletrec";
  "Uprim";
  "Uswitch";
  "Ustaticfail";
  "Ucatch";
  "Utrywith";
  "Uifthenelse";
  "Usequence";
  "Uwhile";
  "Ufor";
  "Uassign";
  "Usend";
]

let closure_ignored =[
  "substitute";
  "bind_params_rec";
  "add_debug_info";
]

let closure_needy_functions = [
  "build_closure_env";
]

let selectgen_ignored = [
]

let selectgen_needy_functions = [
  "join";
  "join_array";
]

let selectgen_methods = [
  "emit_return";
  "bind_let";
  "insert";
  "select_operation";
  "insert_move";
  "insert_moves";
  "insert_op";
  "emit_extcall_args";
  "insert_move_args";
  "insert_move_results";
  "emit_sequence";
]

let dbg_record =  ["exp";"dbi"]

let lam_record = ["lam";"lloc"]
let lambda_constructors = [
  "Lvar";
  "Lconst";
  "Lapply";
  "Lfunction";
  "Llet";
  "Lletrec";
  "Lprim";
  "Lswitch";
  "Lstaticraise";
  "Lstaticcatch";
  "Ltrywith";
  "Lifthenelse";
  "Lsequence";
  "Lwhile";
  "Lfor";
  "Lassign";
  "Lsend";
  "Levent";
  "Lifused";
]

let matching_functions = [
  "split_or";
  "precompile_or";
  "split_constr";
]

let check_all_used name map =
  StringMap.iter (fun v (_, used) ->
    if not !used then
      Printf.eprintf "Warning: patch %s %S not used\n%!" name v
  ) map

let warning source_file cmd =
  Printf.eprintf "Warning: %S uses specialized patch %S\n%!"
    source_file cmd

let rewrite_file map_ast  source_file patches ast =
  match patches with
  | [] -> ast
  | _ ->
    flag_add_dbi := false;
    box_constructors := StringMap.empty;
    ignore_functions := StringMap.empty;
    add_method_arguments := StringMap.empty;
    add_function_arguments := StringMap.empty;
    replace_ident := StringMap.empty;
    add_external_flags := StringMap.empty;
    add_constructor_arguments_pat := StringMap.empty;
    add_constructor_arguments_exp := StringMap.empty;
    add_constructor_arguments_typ := StringMap.empty;
    add_record_fields := StringMap.empty;

    List.iter (fun patch ->
      match patch with
      | PatchNone -> ()

      | PatchAddFunctionArgument { fun_names; fun_new_arguments } ->
        add_to_map add_function_arguments fun_names fun_new_arguments

      | PatchAddConstructorArgumentsPat
          { constr_names; constr_position;
            constr_new_arguments; constr_total_nargs} ->
        add_to_map
          add_constructor_arguments_pat
          constr_names
          (constr_position, constr_new_arguments, constr_total_nargs)

      | PatchAddConstructorArgumentsExp
          { constr_names; constr_position;
            constr_new_arguments; constr_total_nargs } ->
        add_to_map
          add_constructor_arguments_exp
          constr_names
          (constr_position, constr_new_arguments,
           constr_total_nargs)

      | PatchAddConstructorArgumentsTyp
          { constr_names; constr_position;
            constr_new_arguments; constr_total_nargs } ->
        add_to_map
          add_constructor_arguments_typ
          constr_names
          (constr_position, constr_new_arguments,
           constr_total_nargs)

      | PatchAddRecordFields { record_labels; record_new_labels } ->
        add_to_map add_record_fields record_labels record_new_labels

      | PatchBoxConstructors { box_constr_names; box_with_labels } ->
        add_to_map box_constructors box_constr_names box_with_labels

      | PatchAddMethodArguments { meth_names; meth_new_arguments } ->
        add_to_map add_method_arguments meth_names meth_new_arguments

      | PatchReplaceIdents { replace_idents; replace_with } ->
        add_to_map replace_ident replace_idents replace_with

      | PatchAddPrimitiveFlags { prim_flags; prim_names } ->
        add_to_map add_external_flags prim_names prim_flags

      | Patch(cmd, arg, _args) ->
        warning source_file cmd;
        match cmd with
      (* Specific patches *)
        | "box" ->
          begin match arg with
          | "cmm" ->
            add_to_map box_constructors cmm_constructors dbg_record
          | "clambda" ->
            add_to_map box_constructors clambda_constructors dbg_record
          | "lambda" ->
            add_to_map box_constructors lambda_constructors lam_record
          | _ -> assert false
          end

        | "matching.ml" ->
          add_to_map add_function_arguments matching_functions ["loc"]

        | "selectgen.ml" ->
          add_to_map box_constructors cmm_constructors dbg_record;
          flag_add_dbi := true;
          add_to_map ignore_functions selectgen_ignored [];
          add_to_map needy_functions selectgen_needy_functions [];
          add_to_map add_method_arguments selectgen_methods ["dbi"]

        | "selectgen.mli" ->
          add_to_map add_method_arguments selectgen_methods ["Debuginfo.t"]

        | "selection.ml" ->
          add_to_map box_constructors cmm_constructors dbg_record;

        | "printclambda.ml" ->
          add_to_map box_constructors clambda_constructors dbg_record;

        | "printcmm.ml" ->
          add_to_map box_constructors cmm_constructors dbg_record;

        | "cmmgen.ml" ->
          flag_add_dbi := true;
          add_to_map box_constructors cmm_constructors dbg_record;
          add_to_map box_constructors clambda_constructors dbg_record;
          add_to_map ignore_functions cmmgen_ignored [];
          add_to_map needy_functions cmmgen_needy_functions [];

        | "closure.ml" ->
          add_to_map box_constructors clambda_constructors dbg_record;
          flag_add_dbi := true;
          add_to_map ignore_functions closure_ignored [1];
          add_to_map needy_functions closure_needy_functions [1];

        | "" -> () (* empty line *)
        | patch ->
          Printf.eprintf "Warning: Unknown patch %S\n%!" patch;
    ) patches;
    let ast = map_ast ast in
    check_all_used "replace_ident" !replace_ident;
    check_all_used "add_method_arguments" !add_method_arguments;
    check_all_used "add_function_arguments" !add_function_arguments;
    check_all_used "add_external_flags" !add_external_flags;
    check_all_used "add_constructor_arguments_pat" !add_constructor_arguments_pat;
    check_all_used "add_constructor_arguments_exp" !add_constructor_arguments_exp;
    check_all_used "add_constructor_arguments_typ" !add_constructor_arguments_typ;
    check_all_used "add_record_fields" !add_record_fields;
    ast

let rewrite_ml = rewrite_file CmmgenPatch.map_structure
let rewrite_mli = rewrite_file CmmgenPatch.map_signature

      (*
              (*
          begin
            let bindings = args_of_function loc [] fbody in
            let arity = List.length bindings in

            try
              let arg = StringMap.find fname arities.(arity) in
              let bindings = match bindings with
                  [] -> assert false
                | (pat, body) :: bindings ->
                  let body =
                    { body with pexp_desc = Pexp_let(Nonrecursive, [
                      { pat_fname with ppat_desc =
                          Ppat_var {loc with txt= "dbi"}},
                      { body with pexp_desc = Pexp_field(
                        { body with pexp_desc = Pexp_ident {loc with txt=Lident arg}},
                        { loc with txt=Lident "dbi" }
                      )}], body) } in
                  (pat, body) :: bindings
              in
              { str with pstr_desc =
                  Pstr_value(Nonrecursive, [
                    pat_fname,
                    function_of_args fbody bindings]) }
            with Not_found ->
              Printf.eprintf "arity%d %S\n%!"
                arity fname;
              str
            end
              *)
      *)
