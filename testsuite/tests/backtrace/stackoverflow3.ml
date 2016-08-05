let rec iter1 x =
  let a = x+1 in
  let b = x+2 in
  let c = x+3 in
  let d = x+4 in
  let e = x+5 in
  let f = x+6 in
  let g = x+7 in
  let h = x+8 in
  (a,b,c,d,e,f,g,h) :: iter1 x

let _ =
  ignore (iter1 1)
