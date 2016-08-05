These files are pre-compiled, since they cannot easily be cross-compiled.
They are generated under Windows, with ocpwin64-full installed.
ocp-build.exe:
  Go in typerex-private/typerex-build
  Run "rm -rf _obuild; ocp-build init; ocp-build ocp-build"
  And then "cp _obuild/ocp-build/ocp-build.exe ../ocaml/ocpwin/typerex-mingw/"
*_msvc.obj files:
  Go in ocaml/4.01.0+ocp1/cross/flexdll/
  Run "./build_msvc64.sh" (it will build and copy the files)
You should then probably try to:
  Compile a file with ocamlopt
  Compile a file with "ocamlc -custom"
