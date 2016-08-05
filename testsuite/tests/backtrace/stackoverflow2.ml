let rec iter1 x =
  let s = Bytes.create x in
  s :: iter1 x

let _ =
  ignore (iter1 1)
