#!/bin/sh

# We might need to copy byterun/version.h from Linux...

# To be run on Windows in a Git Bash from ocaml/4.01.0+ocp1
# with MSVC 9.0 11.0 and 14.0 installed

ocp-build init

cp -f config/Makefile.msvc64 config/Makefile
cp -f config/s-nt.h config/s.h
cp -f config/m-nt.h config/m.h
# bconfig.ocp, because it must be read before build.ocp...
cp -f cross/stublibs-msvc64/config.ocp.msvc64 bconfig.ocp
rm -f `cat cross/stublibs-msvc64/obj_to_remove.txt`
rm -f `cat cross/stublibs-msvc64/lib_to_remove.txt`

for msvc in 9.0 11.0 12.0 14.0; do
  (
    export OCPWIN_MSVC="export:x64:$msvc"
    ocamlopt -c toto.c && echo OK
    . ./vcvars64.sh
    cp vcvars64.sh vcvars64-$msvc.log
    ocp-build -j 4
    mkdir -p cross/stublibs-msvc64/$msvc
    mv `cat cross/stublibs-msvc64/lib_to_remove.txt` cross/stublibs-msvc64/$msvc
    rm `cat cross/stublibs-msvc64/obj_to_remove.txt`
  ) &> build64-$msvc.log
done
