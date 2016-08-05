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

val serialize_request : out_channel -> compiler_request -> unit

val deserialize_request : in_channel -> compiler_request
