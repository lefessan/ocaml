(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

let arg_list =
  InstallWorker.arg_list
(* @  SubstWorker.arg_list *)

let _ =
  Arg.parse arg_list
    (fun _ -> Arg.usage arg_list InstallWorker.arg_usage)
    InstallWorker.arg_usage
