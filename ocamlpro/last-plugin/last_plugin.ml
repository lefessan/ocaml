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

(* HACK: loading plugins in bytecode (dynlink) is incompatible with
   linking, thus we need to call Symtable.reset after the last plugin
   to avoid bad interactions with linking. *)
let () = Symtable.reset ()
