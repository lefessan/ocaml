(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CSE for IA64 *)

open Mach
open CSEgen

class cse = object (self)

inherit cse_generic

method! is_cheap_operation op =
  match op with
  | Iconst_int n | Iconst_blockheader n -> n <= 65535n && n >= 0n
  | _ -> false

end

let fundecl f =
  (new cse)#fundecl f
