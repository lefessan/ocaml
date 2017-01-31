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

open StringCompat
open Patch_types

let cut_at s c = try Misc.cut_at s c with Not_found -> s,""

#if OCAML_VERSION < "4.04"
let split = Misc.split
#else
  let split s c = String.split_on_char c s
#endif

let read_patches filename =
  let lines = Ocpstd.lines_of_file filename in
  let patches = List.map (fun patch ->
      let cmd, args = cut_at patch ':' in
      let arg, args = cut_at args ':' in
      let args = split args ';' in
      match cmd with

      | "add_function_argument" ->
        PatchAddFunctionArgument
          { fun_new_arguments=[arg];
            fun_names=args }

      | "add_constructor_arguments"
      | "add_constructor_arguments_pat" ->
        PatchAddConstructorArgumentsPat {
          constr_names = args;
          constr_position = End;
          constr_new_arguments = split arg ';';
          constr_total_nargs = -1;
        }

      | "add_constructor_arguments_exp" ->
        PatchAddConstructorArgumentsExp {
          constr_names = args;
          constr_position = End;
          constr_new_arguments = split arg ';';
          constr_total_nargs = -1;
        }

      | "add_constructor_arguments_typ" ->
        PatchAddConstructorArgumentsExp {
          constr_names = args;
          constr_position = End;
          constr_new_arguments = split arg ';';
          constr_total_nargs = -1;
        }

      | "add_record_fields" ->
        PatchAddRecordFields {
          record_labels = args;
          record_new_labels = split arg ';';
        }

      | "box_constructors" ->
        PatchBoxConstructors {
          box_constr_names = split arg ';';
          box_with_labels = args;
        }

      | "add_method_argument" ->
        PatchAddMethodArguments {
          meth_names = split arg ';';
          meth_new_arguments = args;
        }

      | "replace_ident" ->
        PatchReplaceIdents {
          replace_with = arg;
          replace_idents = args;
        }

      | "add_external_flags" ->
        PatchAddPrimitiveFlags {
          prim_names = args;
          prim_flags = split arg ';';
        }

      (* Specific patches *)
      | "box" -> Patch (cmd, arg, args)
      | "" -> PatchNone
      | patch ->
        Printf.eprintf "Warning: Unknown patch %S\n%!" patch;
        PatchNone
  ) lines
  in
  List.iter (function
  | PatchAddRecordFields { record_labels; record_new_labels } ->
    Printf.eprintf "patch %s {\n" "add_record_fields";
    Printf.eprintf "  record_labels = [ \"%s\" ]\n" (String.concat "\" \"" record_labels);
    Printf.eprintf "  record_new_labels = [ \"%s\" ]\n" (String.concat "\" \"" record_new_labels);
    Printf.eprintf "}\n"
  | PatchAddConstructorArgumentsPat { constr_names; constr_new_arguments } ->
    Printf.eprintf "patch %s {\n" "add_constructor_arguments";
    Printf.eprintf "  constr_names = [ \"%s\" ]\n" (String.concat "\" \"" constr_names);
    Printf.eprintf "  constr_new_arguments = [ \"%s\" ]\n" (String.concat "\" \"" constr_new_arguments);
    Printf.eprintf "}\n"
  | PatchAddConstructorArgumentsExp { constr_names; constr_new_arguments } ->
    Printf.eprintf "patch %s {\n" "add_constructor_arguments_exp";
    Printf.eprintf "  constr_exp_names = [ \"%s\" ]\n" (String.concat "\" \"" constr_names);
    Printf.eprintf "  constr_exp_new_arguments = [ \"%s\" ]\n" (String.concat "\" \"" constr_new_arguments);
    Printf.eprintf "}\n"
  | PatchAddConstructorArgumentsTyp { constr_names; constr_new_arguments } ->
    Printf.eprintf "patch %s {\n" "add_constructor_arguments_typ";
    Printf.eprintf "  constr_names = [ \"%s\" ]\n" (String.concat "\" \"" constr_names);
    Printf.eprintf "  constr_new_arguments = [ \"%s\" ]\n" (String.concat "\" \"" constr_new_arguments);
    Printf.eprintf "}\n"
  | PatchReplaceIdents { replace_idents; replace_with } ->
    Printf.eprintf "patch %s {\n" "replace_idents";
    Printf.eprintf "  replace_idents = [ \"%s\" ]\n" (String.concat "\" \"" replace_idents);
    Printf.eprintf "  replace_with = \"%s\"\n" replace_with;
    Printf.eprintf "}\n"
  | PatchAddPrimitiveFlags { prim_names; prim_flags } ->
    Printf.eprintf "patch %s {\n" "add_primitive_flags";
    Printf.eprintf "  prim_flags = [ \"%s\" ]\n" (String.concat "\" \"" prim_flags);
    Printf.eprintf "  prim_names = [ \"%s\" ]\n" (String.concat "\" \"" prim_names);
    Printf.eprintf "}\n"
  | PatchAddFunctionArgument { fun_names; fun_new_arguments } ->
    Printf.eprintf "patch %s {\n" "add_function_arguments";
    Printf.eprintf "  fun_new_arguments = [ \"%s\" ]\n"
      (String.concat "\" \"" fun_new_arguments);
    Printf.eprintf "  fun_names = [ \"%s\" ]\n" (String.concat "\" \"" fun_names);
    Printf.eprintf "}\n"

  | PatchNone -> ()
  | _ ->
    Printf.eprintf "... and other patches...\n%!"
  ) patches
module P = Patch_parser
module L = Patch_lexer

let debug_patch_parser = try
                           ignore (Sys.getenv "OCP_DEBUG_PATCH_PARSER"); true
  with Not_found -> false

let lexer = Patch_lexer.make_lexer
  [ "include"; "patch"; "{";"}";";";"[";"]";"="; "#" ]
let token_of_token = function
    | None -> P.EOF
    | Some token ->
      if debug_patch_parser then begin match token with
      | L.String s ->
        Printf.eprintf "token: string %S\n%!" s
      | L.Kwd s ->
        Printf.eprintf "token: keyword %S\n%!" s
      | L.Ident s ->
        Printf.eprintf "token: ident %S\n%!" s
      | _ ->
        Printf.eprintf "token: other token\n%!"
      end;
      match token with
      | L.String s -> P.STRING s
      | L.Kwd "patch" -> P.PATCH
      | L.Kwd "include" -> P.INCLUDE
      | L.Kwd "=" -> P.EQUAL
      | L.Kwd ";" -> P.SEMI
      | L.Ident ident -> P.IDENT ident
      | L.Kwd "[" -> P.LBRACKET
      | L.Kwd "]" -> P.RBRACKET
      | L.Kwd "{" -> P.LBRACE
      | L.Kwd "}" -> P.RBRACE
      | L.Kwd "#" -> P.SHARP
      | L.Int n -> P.STRING (string_of_int n)
      | L.Float _
      | L.Char _ -> failwith "Unexpected token (int/float/char)"
      | L.Kwd kwd -> Printf.kprintf failwith "Unexpected keyword %S" kwd

type parameter =
| LIST of string list ref
| STRING of string ref

let parse_parameters patch parameters variables =
  let map = ref StringMap.empty in
  let has_default = function
    | LIST r -> !r <> []
    | STRING r -> !r <> ""
  in
  List.iter (fun (var, value) ->
    map := StringMap.add var (ref (has_default value), value) !map
  ) variables;
  let map = !map in
  List.iter (fun (param, value) ->
    try
      let (set, ref) = StringMap.find param map in
      set := true;
      match ref,value with
      | LIST ref, ASTList list -> ref := list
      | STRING ref, ASTString string -> ref := string
      | LIST ref, ASTString string -> ref := [ string ]
      | _ ->
        Printf.eprintf "Error: bad type for parameter %S in patch %S\n%!"
          param patch;
    with Not_found ->
      Printf.eprintf "Error: unknown parameter %S in patch %S\n%!"
        param patch;
      exit 2
  ) parameters;
  StringMap.iter (fun param (set,_) ->
    if not !set then begin
      Printf.eprintf "Error: parameter %S not set in patch %S\n%!"
        param patch;
      exit 2
    end
    ) map

let string_to_argpos str =
  match String.lowercase_ascii str with
  | "start" -> Start
  | "end" -> End
  | _ ->
    begin
      try Pos (int_of_string str)
      with Failure _ ->
        Printf.eprintf "Error: Invalid position argument %s in patch\n%!" str;
        exit 2
    end

let rec read_filename filename =

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let lexer lexbuf =
    token_of_token ( lexer lexbuf )
  in

  let ast =
    try
      Patch_parser.main lexer lexbuf
    with Parsing.Parse_error ->
      let _loc = (Lexing.lexeme_start lexbuf) in
      Printf.eprintf "Error: parse error in %S \n%!" filename;
      read_patches filename;
      exit 2
  in
  close_in ic;

  interp_ast filename ast

and interp_ast filename ast =
  match ast with
  | [] -> []
  | (ASTInclude new_file) :: ast ->
    let next_patches = interp_ast filename ast in
    let filename = Filename.concat (Filename.dirname filename) new_file in
    (read_filename filename) @ next_patches
  | (ASTPatch (patch, parameters)) :: ast->
    let next_patches = interp_ast filename ast in
    let patch = interp_patch filename patch parameters in
    patch :: next_patches

and interp_patch filename patch parameters =
  match patch with
  | "add_method_arguments" ->
    let meth_names = ref [] in
    let meth_new_arguments = ref [] in
    parse_parameters patch parameters [
      "meth_names", LIST meth_names;
      "meth_new_arguments", LIST meth_new_arguments
    ];
    PatchAddMethodArguments { meth_names = !meth_names;
                              meth_new_arguments = !meth_new_arguments }

  | "add_function_argument"
  | "add_function_arguments"
    ->
    let fun_new_argument = ref [] in
    let fun_names = ref [] in
    parse_parameters patch parameters [
      "fun_new_argument", LIST fun_new_argument;
      "fun_names", LIST fun_names;
    ];
    PatchAddFunctionArgument {
      fun_new_arguments = !fun_new_argument;
      fun_names = !fun_names;
    }

  | "add_constructor_arguments"
  | "add_constructor_arguments_pat" ->
    let constr_names = ref [] in
    let constr_position = ref "end" in
    let constr_new_arguments = ref [] in
    let constr_total_nargs = ref "-1" in
    parse_parameters patch parameters [
      "constr_names", LIST constr_names;
      "constr_position", STRING constr_position;
      "constr_new_arguments", LIST constr_new_arguments;
      "constr_prev_nargs", STRING constr_total_nargs;
    ];
    PatchAddConstructorArgumentsPat {
      constr_names = !constr_names;
      constr_position = string_to_argpos !constr_position;
      constr_new_arguments = !constr_new_arguments;
      constr_total_nargs = int_of_string !constr_total_nargs;
    }

  | "add_constructor_arguments_exp" ->
    let constr_names = ref [] in
    let constr_position = ref "end" in
    let constr_new_arguments = ref [] in
    let constr_total_nargs = ref "-1" in
    parse_parameters patch parameters [
      "constr_names", LIST constr_names;
      "constr_position", STRING constr_position;
      "constr_new_arguments", LIST constr_new_arguments;
      "constr_prev_nargs", STRING constr_total_nargs;
    ];
    PatchAddConstructorArgumentsExp {
      constr_names = !constr_names;
      constr_position = string_to_argpos !constr_position;
      constr_new_arguments = !constr_new_arguments;
      constr_total_nargs = int_of_string !constr_total_nargs;
    }

  | "add_constructor_arguments_typ" ->
    let constr_names = ref [] in
    let constr_position = ref "end" in
    let constr_new_arguments = ref [] in
    let constr_total_nargs = ref "-1" in
    parse_parameters patch parameters [
      "constr_names", LIST constr_names;
      "constr_position", STRING constr_position;
      "constr_new_arguments", LIST constr_new_arguments;
      "constr_prev_nargs", STRING constr_total_nargs;
    ];
    PatchAddConstructorArgumentsTyp {
      constr_names = !constr_names;
      constr_position = string_to_argpos !constr_position;
      constr_new_arguments = !constr_new_arguments;
      constr_total_nargs = int_of_string !constr_total_nargs;
    }

  | "add_record_fields" ->
    let record_labels = ref [] in
    let record_new_labels = ref [] in
    parse_parameters patch parameters [
      "record_labels", LIST record_labels;
      "record_new_labels", LIST record_new_labels;
    ];
    PatchAddRecordFields {
      record_labels = !record_labels;
      record_new_labels = !record_new_labels;
    }

  | "box_constructors" ->
    let box_constr_names = ref [] in
    let box_with_labels = ref [] in
    parse_parameters patch parameters [
      "box_constr_names", LIST box_constr_names;
      "box_with_labels", LIST box_with_labels;
    ];
    PatchBoxConstructors {
      box_constr_names = !box_constr_names;
      box_with_labels = !box_with_labels;
    }

  | "replace_idents" ->
    let replace_idents = ref [] in
    let replace_with = ref "" in
    parse_parameters patch parameters [
      "replace_idents", LIST replace_idents;
      "replace_with", STRING replace_with;
    ];
    PatchReplaceIdents {
      replace_idents = !replace_idents;
      replace_with = !replace_with;
    }

  | "add_primitive_flags" ->
    let prim_names = ref [] in
    let prim_flags = ref [] in
    parse_parameters patch parameters [
      "prim_names", LIST prim_names;
      "prim_flags", LIST prim_flags;
    ];
    PatchAddPrimitiveFlags {
      prim_names = !prim_names;
      prim_flags = !prim_flags;
    }

  | _ ->
    Printf.eprintf "Error: unknown patch %S in %S\n%!" patch filename;
    exit 2

let patches = ref []

let rewriter f { Misc.sourcefile } ast =
  (*      if verbose then
          Printf.eprintf "Patching %S\n%!" sourcefile; *)
  let external_patches =
    let filename = sourcefile ^ ".ocpp" in
    if Sys.file_exists filename then
      read_filename filename else []
  in
  let more_patches =
    let filename = sourcefile ^ ".mlsp" in
    if Sys.file_exists filename then
      read_filename filename else []
  in
  let all_patches = external_patches @ more_patches @ !patches in
  patches := [];
  f sourcefile all_patches ast

let () =
  (*  Printf.eprintf "register ocpPatch rewriters\n%!"; *)
  Pparse.ImplementationHooks.add_hook "patch"
    (rewriter Patch_engine.rewrite_ml);
  Pparse.InterfaceHooks.add_hook "patch"
    (rewriter Patch_engine.rewrite_mli);
  Ocpp.add_option "Patches" (fun _name _v ->
    Printf.eprintf "#Patches obsoleted\n%!";
    exit 2
  (*    patches := v :: !patches *)
  );

  ()

let init () = ()
