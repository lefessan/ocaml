
module Perv : sig
(** {6 Debugging} *)

  external __LOC__ : string = "%loc_LOC"
  external __FILE__ : string = "%loc_FILE"
  external __LINE__ : int = "%loc_LINE"
  external __MODULE__ : string = "%loc_MODULE"
  external __POS__ : string * int * int * int = "%loc_POS"

  external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
  external __FILE_OF__ : 'a -> string * 'a = "%loc_FILE"
  external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
  external __MODULE_OF__ : 'a -> string * 'a = "%loc_MODULE"
  external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

end

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
