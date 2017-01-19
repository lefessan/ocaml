
let _ = print_string "Hello"; print_newline ()
#ifdef HAS_OCPP
let _ = print_string "Has OCPP !"; print_newline ()
#else
let _ = print_string "no OCPP ????"; print_newline ()
#endif
let _ = print_string "Good bye"; print_newline ()

let _ = print_string "Hello again"; print_newline ()
#if OCAML_VERSION > "3.12.1"
let _ = print_string "OCaml Version > 3.12.1 !"; print_newline ()
#else
let _ = print_string "OCaml Version <= 3.12.1CPP ????"; print_newline ()
#endif
let _ = print_string "Good bye"; print_newline ()
