let _ =
  Printexc.record_backtrace true;
  let rec iter n =
    1 + iter (n-1)
  in
  iter 100000
