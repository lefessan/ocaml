
(* include Myocamlbuild_config *)

let prefix =
  if ocp_relocatable then
    Filename.dirname (Filename.dirname Sys.executable_name)
  else
    prefix
let bindir =
  if ocp_relocatable then
    Filename.concat prefix "bin"
  else
    bindir
let libdir =
  if ocp_relocatable then
    Filename.concat prefix ocamllibdir
  else
    libdir
