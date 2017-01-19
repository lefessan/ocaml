
module Sys : sig

  val system : string -> int
(** Execute the given shell command and return its exit code.
    Differs from Sys.command on Windows by adding quotes around
    the command before calling "cmd".
 *)

end

val iteri_lines : (int -> string -> unit) -> string -> unit
val lines_of_file : string -> string list
val find_in_path : string list -> string -> string
val cut_at : string -> char -> string * string
