
let () =
  (* TODO: change location *)
  Printexc.record_backtrace true;
  Ocpp.register ()
