(* This is a minimal bytecode wrapper, that can be added to any native
   program so that running "ocamlrun program" will actually be equivalent
   to calling "program".

   We use "ocp-byteclean -only-bytecode" to remove the MEMP section
   and not output the camlheader.

   In order for ocamlrun to be able to execute it, ocamlrun should be
   able to find dllunix.dll. This is only possible if:
   * ocamlrun is a wrapper and correctly set CAML_LD_LIBRARY_PATH.
   * ocamlrun is not a wrapper and dllunix.dll is in the PATH.
*)



module Unix = struct

type error =
  (* Errors defined in the POSIX standard *)
    E2BIG               (* Argument list too long *)
  | EACCES              (* Permission denied *)
  | EAGAIN              (* Resource temporarily unavailable; try again *)
  | EBADF               (* Bad file descriptor *)
  | EBUSY               (* Resource unavailable *)
  | ECHILD              (* No child process *)
  | EDEADLK             (* Resource deadlock would occur *)
  | EDOM                (* Domain error for math functions, etc. *)
  | EEXIST              (* File exists *)
  | EFAULT              (* Bad address *)
  | EFBIG               (* File too large *)
  | EINTR               (* Function interrupted by signal *)
  | EINVAL              (* Invalid argument *)
  | EIO                 (* Hardware I/O error *)
  | EISDIR              (* Is a directory *)
  | EMFILE              (* Too many open files by the process *)
  | EMLINK              (* Too many links *)
  | ENAMETOOLONG        (* Filename too long *)
  | ENFILE              (* Too many open files in the system *)
  | ENODEV              (* No such device *)
  | ENOENT              (* No such file or directory *)
  | ENOEXEC             (* Not an executable file *)
  | ENOLCK              (* No locks available *)
  | ENOMEM              (* Not enough memory *)
  | ENOSPC              (* No space left on device *)
  | ENOSYS              (* Function not supported *)
  | ENOTDIR             (* Not a directory *)
  | ENOTEMPTY           (* Directory not empty *)
  | ENOTTY              (* Inappropriate I/O control operation *)
  | ENXIO               (* No such device or address *)
  | EPERM               (* Operation not permitted *)
  | EPIPE               (* Broken pipe *)
  | ERANGE              (* Result too large *)
  | EROFS               (* Read-only file system *)
  | ESPIPE              (* Invalid seek e.g. on a pipe *)
  | ESRCH               (* No such process *)
  | EXDEV               (* Invalid link *)
  (* Additional errors, mostly BSD *)
  | EWOULDBLOCK         (* Operation would block *)
  | EINPROGRESS         (* Operation now in progress *)
  | EALREADY            (* Operation already in progress *)
  | ENOTSOCK            (* Socket operation on non-socket *)
  | EDESTADDRREQ        (* Destination address required *)
  | EMSGSIZE            (* Message too long *)
  | EPROTOTYPE          (* Protocol wrong type for socket *)
  | ENOPROTOOPT         (* Protocol not available *)
  | EPROTONOSUPPORT     (* Protocol not supported *)
  | ESOCKTNOSUPPORT     (* Socket type not supported *)
  | EOPNOTSUPP          (* Operation not supported on socket *)
  | EPFNOSUPPORT        (* Protocol family not supported *)
  | EAFNOSUPPORT        (* Address family not supported by protocol family *)
  | EADDRINUSE          (* Address already in use *)
  | EADDRNOTAVAIL       (* Can't assign requested address *)
  | ENETDOWN            (* Network is down *)
  | ENETUNREACH         (* Network is unreachable *)
  | ENETRESET           (* Network dropped connection on reset *)
  | ECONNABORTED        (* Software caused connection abort *)
  | ECONNRESET          (* Connection reset by peer *)
  | ENOBUFS             (* No buffer space available *)
  | EISCONN             (* Socket is already connected *)
  | ENOTCONN            (* Socket is not connected *)
  | ESHUTDOWN           (* Can't send after socket shutdown *)
  | ETOOMANYREFS        (* Too many references: can't splice *)
  | ETIMEDOUT           (* Connection timed out *)
  | ECONNREFUSED        (* Connection refused *)
  | EHOSTDOWN           (* Host is down *)
  | EHOSTUNREACH        (* No route to host *)
  | ELOOP               (* Too many levels of symbolic links *)
  | EOVERFLOW
  (* All other errors are mapped to EUNKNOWNERR *)
  | EUNKNOWNERR of int  (* Unknown error *)

(* Initialization *)

  external startup: unit -> unit = "unix_startup"
  external cleanup: unit -> unit = "unix_cleanup"

  let _ = if Sys.win32 then begin
    startup();
  (*    at_exit cleanup *)
  end

exception Unix_error of error * string * string

let _ = Callback.register_exception "Unix.Unix_error"
                                    (Unix_error(E2BIG, "", ""))

type wait_flag =
    WNOHANG
  | WUNTRACED

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
external waitpid : wait_flag list -> int -> int * process_status
                 = "unix_waitpid"

type file_descr

module WinFD = struct
external of_fd : int -> file_descr = "unix_handle_fd"
end

module UnixFD = struct
external of_fd : int -> file_descr = "%identity"
end

let filedescr_of_fd fd =
  if Sys.win32 then WinFD.of_fd fd else UnixFD.of_fd fd

let stdin = filedescr_of_fd 0
let stdout = filedescr_of_fd 1
let stderr = filedescr_of_fd 2

external win_create_process : string -> string -> string option ->
                              file_descr -> file_descr -> file_descr -> int
                            = "win_create_process" "win_create_process_native"

let make_cmdline args =
  let maybe_quote f =
    if String.contains f ' ' || String.contains f '\"'
    then Filename.quote f
    else f in
  String.concat " " (List.map maybe_quote (Array.to_list args))

let create_process prog args fd1 fd2 fd3 =
  win_create_process prog (make_cmdline args) None fd1 fd2 fd3

end

module OnlyWin32 = struct

  let rec waitpid1 pid =
    let (_, status) = Unix.waitpid [] pid in
    match status with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n -> -n
    | Unix.WSTOPPED n -> -1000-n

  let rec safe_waitpid pid =
    try
      waitpid1 pid
    with Unix.Unix_error (Unix.EINTR, _, _) -> safe_waitpid pid

  let command cmd argv =
    let pid = try
                Unix.create_process cmd argv
                  Unix.stdin Unix.stdout Unix.stderr
      with e ->
(*        Printf.fprintf Pervasives.stderr "Error \"%s\" executing %s\n%!"
          (Printexc.to_string e) cmd; *)
        exit 2
    in
    let status = safe_waitpid pid in
  (*    Printf.fprintf stderr "waitpid returned %d\n%!" status; *)
    status

  let simulate_exec cmd argv =
    let status = command cmd argv in
    exit status

end

let print_args () =
  (*
  Printf.printf "bytecode called as:\n%!";
  Array.iteri (fun i arg ->
    Printf.printf "   arg[%d] = %S\n%!" i arg
  ) Sys.argv
  *)
  ()

let _ =
  print_args ();
  let args = Sys.argv in
  let cmd = args.(0) in
  OnlyWin32.simulate_exec cmd args
