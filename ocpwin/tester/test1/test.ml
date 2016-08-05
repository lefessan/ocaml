
external f : int -> int = "test_f"

let _ =
  assert (f 3 = 3)

module S = Str
module U = Unix
module B = Bigarray
module D = Dynlink
module G = Graphics
module N = Big_int
