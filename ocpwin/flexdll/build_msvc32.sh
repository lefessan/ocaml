#!/bin/sh

rm -f flexdll.o flexdll.obj
rm -f flexdll_initer.o flexdll_initer.obj
rm -f flexdll_ocaml.o flexdll_ocaml.obj

export OCAMLLIB=`ocamlc -where`
export OCPCOMP_FILE=$OCAMLLIB/ocaml.msvc32
echo $OCPCOMP_FILE

ocamlc -ccopt -DMSVC -verbose flexdll.c &&
mv flexdll.o flexdll_msvc.obj &&
ocamlc -ccopt -DMSVC -verbose flexdll_initer.c &&
mv flexdll_initer.o flexdll_initer_msvc.obj &&
ocamlc -ccopt -DMSVC -verbose flexdll_ocaml.c &&
mv flexdll_ocaml.o flexdll_ocaml_msvc.obj

mv flexdll_msvc.obj \
   flexdll_initer_msvc.obj \
   flexdll_ocaml_msvc.obj \
   ../typerex-mingw/

echo Files moved to ../typerex-mingw
