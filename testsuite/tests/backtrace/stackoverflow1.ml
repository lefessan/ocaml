let rec iter1 x =
  x :: iter2 (x+1)

and iter2 y =
  (y+1) :: iter1 y

let _ =
  ignore (iter1 1)
