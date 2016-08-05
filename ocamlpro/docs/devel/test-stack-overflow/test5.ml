let _ =
  Printexc.record_backtrace true;
  let rec iter1 n =
    1 + iter2 (n-1)

  and iter2 n =
    1 - iter1 (n-1)
  in
  try
    iter1 100000
  with _ ->
    let r = Printexc.get_raw_backtrace () in
    Printexc.print_raw_backtrace stdout r;
    1
