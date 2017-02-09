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

module Sys = struct

  include Sys

  let system cmdline =
    Sys.command (
      (* Under Windows, system argument is sent to 'cmd' without the quotes,
         so we need to add them before !*)
      if Sys.win32 then "\"" ^ cmdline ^ "\"" else cmdline)

end

let iteri_lines f filename =
    let ic = open_in filename in
    let linenum = ref 0 in
    try
      while true do
        incr linenum;
        f !linenum (input_line ic)
      done;
      assert false
    with End_of_file ->
      close_in ic


let lines_of_file filename =
  let lines = ref [] in
  iteri_lines (fun _i line -> lines := line :: !lines) filename;
  List.rev !lines

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

    (* TODO
module MarshalWithLocid = struct

  type extern_flags =
      No_sharing
    | Closures
    | Compat_32
    | Keep_locid

  external to_channel: out_channel -> 'a -> extern_flags list -> unit
    = "caml_output_value"
  external to_string: 'a -> extern_flags list -> string
    = "caml_output_value_to_string" "!memprof"
  external to_buffer_unsafe:
    string -> int -> int -> 'a -> extern_flags list -> int
    = "caml_output_value_to_buffer"

  let to_buffer buff ofs len v flags =
    if ofs < 0 || len < 0 || ofs > String.length buff - len
    then invalid_arg "Marshal.to_buffer: substring out of bounds"
    else to_buffer_unsafe buff ofs len v flags

end
    *)
