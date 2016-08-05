
module Perv = struct

 (* Debugging *)

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
