#!/bin/sh

rm -f flexdll.o flexdll.obj
rm -f flexdll_initer.o flexdll_initer.obj
rm -f flexdll_ocaml.o flexdll_ocaml.obj

export OCAMLLIB=`ocamlc -where`
export OCPCOMP_FILE=$OCAMLLIB/ocaml.msvc64
echo $OCPCOMP_FILE

ocamlc -ccopt " /Z7 /GS-" -verbose -ccopt -DMSVC -verbose flexdll.c &&
mv flexdll.o flexdll_msvc64.obj &&
ocamlc -ccopt " /Z7 /GS-" -verbose -ccopt -DMSVC -verbose flexdll_initer.c &&
mv flexdll_initer.o flexdll_initer_msvc64.obj &&
ocamlc -ccopt " /Z7 /GS-" -verbose -ccopt -DMSVC -verbose flexdll_ocaml.c &&
mv flexdll_ocaml.o flexdll_ocaml_msvc64.obj

mv flexdll_msvc64.obj \
   flexdll_initer_msvc64.obj \
   flexdll_ocaml_msvc64.obj \
   ../typerex-mingw64

echo Files moved to ../typerex-mingw64
