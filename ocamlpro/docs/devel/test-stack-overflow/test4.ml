let f () =
  let rec iter n =
    1 + iter (n-1)
  in
  ignore (iter 100000);
  2

let _ =
  Printexc.record_backtrace true;
  try
    print_int (f())
  with _ ->
    let r = Printexc.get_raw_backtrace () in
    Printexc.print_raw_backtrace stdout r
