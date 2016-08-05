
module Sys = struct

let system cmdline =
  Sys.command (
    (* Under Windows, system argument is sent to 'cmd' without the quotes,
       so we need to add them before !*)
    if Sys.win32 then "\"" ^ cmdline ^ "\"" else cmdline)

end

module Arg = struct

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Bool of (bool -> unit)     (* Call the function with a bool argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (* Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (* Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (* Set the reference to the float argument *)
  | Tuple of spec list         (* Take several arguments according to the
                                  spec list *)
  | Symbol of string list * (string -> unit)
                               (* Take one of the symbols as argument and
                                  call the function with the symbol. *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

  let default_arglist = ref []

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
  iteri_lines (fun i line -> lines := line :: !lines) filename;
  List.rev !lines

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
