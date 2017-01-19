

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

  Clflags.arg_spec :=

    ("-D", Arg.String (fun s ->
      Ocpp.add_macro s (try [Ocpp_version.mk_string (snd (Misc.cut_at s '='))]
        with Not_found -> [])),
     "<macro[=string]> Define a macro for Ocpp") ::

    !Clflags.arg_spec;
  ()

let init () = ()
