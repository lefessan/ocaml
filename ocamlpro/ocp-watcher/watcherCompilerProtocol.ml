(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2015 OCamlPro                                                 *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

type context = string array * string * int

type comp_struct =
  {
    cs_source: string;
    cs_prod: (string * string option) list;
    cs_dep: (string * string option) list;
    cs_cmd: context;
  }

type link_struct =
  {
    ls_outputs: (string * string option) list;
    ls_objs: (string * string option) list;
    ls_cmd: context;
  }

type compiler_request =
  | Reg_ml of comp_struct
  | Reg_mli of comp_struct
  | Reg_lib of link_struct
  | Reg_pack of link_struct
  | Reg_exe of link_struct

exception Deserialization_error of string

open StringCompat

let error s = raise (Deserialization_error s)

let s_int oc i =
  (* Assume we don't write integers bigger than 2^30 *)
  output_binary_int oc i

let d_int ic =
  input_binary_int ic

let s_string oc str =
  let i = String.length str in
  output_char oc 's';
  s_int oc i;
  output_string oc str

let d_string ic =
  let control = input_char ic in
  if control <> 's'
  then error (Printf.sprintf "Expecting character 's', got %C" control)
  else
    let len = input_binary_int ic in
    really_input_string ic len

let s_opt out_fun oc opt =
  match opt with
  | None ->
    output_char oc 'N'
  | Some v ->
    begin
      output_char oc 'S';
      out_fun oc v
    end

let d_opt in_fun ic =
  let control = input_char ic in
  if control = 'N'
  then None
  else if control = 'S'
  then Some (in_fun ic)
  else error (Printf.sprintf "Expecting character 'N' or 'S', got %C" control)

let s_list out_fun oc l =
  let rec aux = function
    | [] -> ()
    | x :: tail ->
      begin
        out_fun oc x;
        aux tail
      end
  in
  output_char oc 'l';
  output_binary_int oc (List.length l);
  aux l

let d_list in_fun ic =
  let control = input_char ic in
  if control <> 'l'
  then error (Printf.sprintf "Expecting character 'l', got %C" control)
  else
    let len = input_binary_int ic in
    let rec aux acc n =
      if n = 0 then List.rev acc
      else
        let x = in_fun ic in
        aux (x::acc) (n-1)
    in
    aux [] len

let s_strlist = s_list s_string

let d_strlist = d_list d_string

let s_strolist =
  s_list
    (fun oc (str, crco) ->
       s_string oc str;
       s_opt s_string oc crco)

let d_strolist =
  d_list
    (fun ic ->
       let str = d_string ic in
       let crco = d_opt d_string ic in
       (str, crco))

let s_context oc (argv,dir,index) =
  output_char oc 'x';
  s_strlist oc (Array.to_list argv);
  s_string oc dir;
  s_int oc index

let d_context ic =
  let control = input_char ic in
  if control <> 'x'
  then error (Printf.sprintf "Expecting character 'x', got %C" control)
  else
    let argv = Array.of_list (d_strlist ic) in
    let dir = d_string ic in
    let index = d_int ic in
    (argv, dir, index)

let s_comp oc cs =
  output_char oc 'c';
  s_string oc cs.cs_source;
  s_strolist oc cs.cs_prod;
  s_strolist oc cs.cs_dep;
  s_context oc cs.cs_cmd

let d_comp ic =
  let control = input_char ic in
  if control <> 'c'
  then error (Printf.sprintf "Expecting character 'c', got %C" control)
  else
    let cs_source = d_string ic in
    let cs_prod = d_strolist ic in
    let cs_dep = d_strolist ic in
    let cs_cmd = d_context ic in
    {cs_source; cs_prod; cs_dep; cs_cmd}

let s_link oc ls =
  output_char oc 'k';
  s_strolist oc ls.ls_outputs;
  s_strolist oc ls.ls_objs;
  s_context oc ls.ls_cmd

let d_link ic =
  let control = input_char ic in
  if control <> 'k'
  then error (Printf.sprintf "Expecting character 'k', got %C" control)
  else
    let ls_outputs = d_strolist ic in
    let ls_objs = d_strolist ic in
    let ls_cmd = d_context ic in
    {ls_outputs; ls_objs; ls_cmd}

let serialize_request oc req =
  match req with
  | Reg_ml cs ->
    s_string oc "IMPL";
    s_comp oc cs
  | Reg_mli cs ->
    s_string oc "INTF";
    s_comp oc cs
  | Reg_lib ls ->
    s_string oc "LIBR";
    s_link oc ls
  | Reg_pack ls ->
    s_string oc "PACK";
    s_link oc ls
  | Reg_exe ls ->
    s_string oc "EXEC";
    s_link oc ls

let deserialize_request ic =
  try
    let kind = d_string ic in
    match kind with
    | "IMPL" -> Reg_ml (d_comp ic)
    | "INTF" -> Reg_mli (d_comp ic)
    | "LIBR" -> Reg_lib (d_link ic)
    | "PACK" -> Reg_pack (d_link ic)
    | "EXEC" -> Reg_exe (d_link ic)
    | _ -> error (Printf.sprintf "Unkown request kind %S" kind)
  with
  | End_of_file -> error "Unexpected end of file while deserializing"
