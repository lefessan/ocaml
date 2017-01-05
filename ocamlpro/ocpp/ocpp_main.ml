

let () =

  (* TODO: change location *)
  Printexc.record_backtrace true;

  let macros =
    if !Clflags.native_code then
      [ "OCAML_NATIVE" ]
    else
      [ "OCAML_BYTECODE" ]
  in
  Ocpp.register ();
  List.iter (fun macro ->
    Ocpp.add_macro macro []) macros;
  ()

let init () = ()
