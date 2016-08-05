(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of primitive functions *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_memprof: bool;        (* Result only is allocated *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool }  (* Does the above operate on unboxed floats? *)

let ppf = Format.err_formatter

let parse_declaration loc arity decl =
  let rec iter p = function
      | [] ->
      if p.prim_name = "" then begin
        Format.fprintf ppf "%a\nError: invalid primitive declaration@."
          Location.print loc;
        exit 2
      end;
      p
    | "!memprof" :: rem -> iter { p with prim_memprof = true } rem
    | "!noalloc" :: rem -> iter { p with prim_alloc = false } rem
    | "noalloc" :: rem -> iter { p with prim_alloc = false } rem
    | "float" :: rem -> iter { p with prim_native_float = true } rem
    | name :: rem ->
      if p.prim_name = "" then
        iter { p with prim_name = name } rem
      else
      if p.prim_native_name = "" then
        iter { p with prim_native_name = name } rem
      else begin
        Format.fprintf ppf "%a\nWarning: extra name %S in primitive declaration@." Location.print loc name;
        iter p rem
      end
  in
  iter { prim_name = ""; prim_arity = arity; prim_alloc = true;
         prim_memprof = false;
         prim_native_float = false; prim_native_name = "" } decl

let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list = if not p.prim_memprof then "!memprof" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list = if p.prim_native_float then "float" :: list else list in
  List.rev list

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name
