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

open Parser

(*
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
*)

let mk_string s = STRING (s,None)
let get_STRING = function
  | STRING (s,_) -> s
  | _ -> assert false

let name_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "STRING(%S,_)" s
  | INT (int, s) ->
      Printf.sprintf "INT(%s,%s)" int
        (match s with None -> "None" | Some c -> Printf.sprintf "Some %c" c)
  | FLOAT (float, s) ->
      Printf.sprintf "FLOAT(%s,%s)" float
        (match s with None -> "None" | Some c -> Printf.sprintf "Some %c" c)
  | HASH -> "HASH"
  | _  -> assert false

let string_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "%S" s
  | _  -> assert false

let token_of_token = function
  | STRING (s,_) -> Ocpp_parser.STRING s
  | _ -> assert false

(*
#endif
*)
