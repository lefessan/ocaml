let _ =
  Printexc.record_backtrace true ;
 let rec iter n =
    1 + iter (n-1)
  in
  try
    iter 100000
  with _ -> raise Not_found
