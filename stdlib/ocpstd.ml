
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
