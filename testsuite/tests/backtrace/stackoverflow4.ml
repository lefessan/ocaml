let rec iter1 x =
  let s1 = Bytes.create x in
  let s2 = Bytes.create x in
  s1 :: s2 :: iter1 x

let _ =
  ignore (iter1 1)
