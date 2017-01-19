
module Sys : sig

val system : string -> int
(** Execute the given shell command and return its exit code.
    Differs from Sys.command on Windows by adding quotes around
    the command before calling "cmd".
 *)

end

val iteri_lines : (int -> string -> unit) -> string -> unit
val lines_of_file : string -> string list

  (*
module Arg : sig

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

  val default_arglist : (string * spec * string) list ref
end
  *)

(* TODO
module MarshalWithLocid : sig

  type extern_flags =
      No_sharing                    (** Don't preserve sharing *)
    | Closures                      (** Send function closures *)
    | Compat_32                     (** Ensure 32-bit compatibility *)
    | Keep_locid                    (** Preserve [locid] in marshalled data *)
  (** The flags to the [Marshal.WithLocId.to_*] functions below. *)

  val to_channel : out_channel -> 'a -> extern_flags list -> unit
  (** See[Marshal.to_channel]. *)

  external to_string :
  'a -> extern_flags list -> string = "caml_output_value_to_string" "!memprof"
  (** See [Marshal.to_string] *)

  val to_buffer : string -> int -> int -> 'a -> extern_flags list -> int
  (** See [Marshal.to_buffer] *)

end
  *)
