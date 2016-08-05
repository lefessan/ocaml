let _ =
  Printexc.record_backtrace true;
  let rec iter1 n =
    1 + iter2 (n-1)

  and iter2 n =
    1 - iter3 (n-1)

  and iter3 n =
    1 - iter1 (n-1)
  in
  iter1 100000
