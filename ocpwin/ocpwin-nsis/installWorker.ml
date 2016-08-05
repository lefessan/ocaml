(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

open InstallMisc
open InstallPre
open InstallPost

let arg_usage = Printf.sprintf " %s OPTIONS : post-installer for Windows"
  Sys.argv.(0)

let arg_list = [
(* Removed, to avoid anybody using it to recreate an installer...

  "-merge-windows", Arg.String merge_windows, " <dirname> : merge dirs";
  "-pre-install", Arg.String pre_install,
  String.concat "\n\t" [
    " <dirname> : prepare for installation. <dirname> is the name of the";
    "directory containing the files to be installed."
  ];
  "-no-split-bytecode", Arg.Clear split_bytecode, " : don't split bytecode files";
  "-no-load-libraries", Arg.Clear load_libraries, " : don't compress libraries";
*)
  "-post-install", Arg.String post_install,
  String.concat "\n\t" [
    " <dirname> : rebuild after installation. <dirname> is the name of the";
    "directory where the files have been installed.";
  ];
]
