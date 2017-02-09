(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* output of the parser *)
type ast = definition list

and definition =
| ASTInclude of string
| ASTPatch of string * (string * value) list
and value =
| ASTString of string
| ASTList of string list

type argpos = Start | End | Pos of int

(* output after interpretation *)
type patches = patch list

and patch =
| PatchNone
| PatchAddFunctionArgument of add_function_argument
| PatchAddConstructorArgumentsPat of add_constructors_arguments
| PatchAddConstructorArgumentsExp of add_constructors_arguments
| PatchAddConstructorArgumentsTyp of add_constructors_arguments
| PatchAddRecordFields of add_record_fields
| PatchBoxConstructors of box_constructors
| PatchAddMethodArguments of add_method_arguments
| PatchReplaceIdents of replace_idents
| PatchAddPrimitiveFlags of add_primitive_flags
| Patch of string * string * string list

and add_function_argument = {
  fun_new_arguments : string list;
  fun_names : string list;
}

and add_constructors_arguments = {
  constr_names : string list;
  constr_position : argpos;
  constr_new_arguments : string list;
  constr_total_nargs : int;
}

and add_record_fields = {
  record_labels : string list;
  record_new_labels : string list;
}

and box_constructors = {
  box_constr_names : string list;
  box_with_labels : string list;
}

and add_method_arguments = {
  meth_names : string list;
  meth_new_arguments : string list;
}

and replace_idents = {
  replace_idents : string list;
  replace_with : string;
}

and add_primitive_flags = {
  prim_names : string list;
  prim_flags : string list;
}
