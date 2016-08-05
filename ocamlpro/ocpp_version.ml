(***********************************************************************)
(*                                                                     *)
(*                              TypeRex                                *)
(*                                                                     *)
(*  Copyright 2014, Fabrice Le Fessant, OCamlPro/INRIA.                *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

open Parser

#if OCAML_VERSION = "4.01.0+ocp1"

let mk_string s = STRING s
let get_STRING = function
  | STRING s -> s
  | _ -> assert false

let name_of_token = function
  | STRING s -> Printf.sprintf "STRING %S" s
  | _  -> assert false

let string_of_token = function
  | STRING s -> Printf.sprintf "%S" s
  | _  -> assert false

let token_of_token = function
  | STRING s -> Ocpp_parser.STRING s
  | _ -> assert false

#else

let mk_string s = STRING (s,None)
let get_STRING = function
  | STRING (s,_) -> s
  | _ -> assert false

let name_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "STRING(%S,_)" s
  | _  -> assert false

let string_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "%S" s
  | _  -> assert false

let token_of_token = function
  | STRING (s,_) -> Ocpp_parser.STRING s
  | _ -> assert false

#endif
