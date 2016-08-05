
(* patch: internal hooks for parsetrees *)
exception RewriterExit of exn
let fold_rewriters list sourcefile ast =
  List.fold_left (fun ast (name,f) ->
    try
      f sourcefile ast
    with
    | RewriterExit e -> raise e
    | e ->
      Printf.eprintf "Error: exception %S while running rewriter %S\n%!"
        (Printexc.to_string e) name;
      exit 2
  ) ast (List.sort compare list)

module Clflags = struct

  let _ =
    try Clflags.runtime_variant := Sys.getenv "OCAML_RUNTIME_VARIANT"
    with Not_found -> ()

  let internal_parsetree_rewriters = ref []
  let internal_typedtree_rewriters = ref []

  (* dconcrete in OCP_COMPILER_DEBUG *)
  let dump_concrete = ref false

  (* tryocaml: wrap constants with translation functions *)
  let wrap_constants = ref false

  (* Include location in exe (or leave in a .cmg of [false]) *)
  let include_location_table = ref true

end

let parse_intf = ref false
