
let register_extensions ?(macros=[]) () =
  Ocpp.register ();
  List.iter (fun macro ->
    Ocpp.add_macro macro []) macros;

  Needhcons.init ();

  begin (* For Tailrec *)
    try
      ignore (Sys.getenv "OCP_CHECKTAIL");
      Ocpp.add_internal_implementation_rewriter "tailrec"
        (fun sourcefile impl -> Tailrec.structure Tailrec.empty_env impl; impl)
    with Not_found -> ()
  end;

  Patch_main.register ();

  ()
