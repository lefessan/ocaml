#!/bin/bash

# Edit the for loop to add/remove OCaml versions to compile

ocpwin=`dirname $0`
cd $ocpwin

versions=$*

if [ "x$versions" = "x" ]; then
    echo Build for all versions
    versions="4.01.0+ocp1 4.02.1+ocp1"
else
    echo Build for $versions
fi


for version in $versions ; do

    ORIGDIR=../$version

    rm -rf $ORIGDIR/cross/nsis/files
    rm -rf $ORIGDIR/cross/local
    rm -f $ORIGDIR/cross/nsis/ocpwin*exe
   
   TEMPDIR=/tmp/ocpwin-release-$version

   rm -rf $TEMPDIR
   mkdir -p $TEMPDIR
   echo 'include ocpwin/Makefile.release' > $TEMPDIR/Makefile
   
   mkdir -p $TEMPDIR/ocpwin32
   mkdir -p $TEMPDIR/ocpwin64
   mkdir -p $TEMPDIR/ocamlpro
   mkdir -p $TEMPDIR/ocpwin
   
   rsync -auv ../$version/. $TEMPDIR/ocpwin32/.
   rsync -auv ../$version/. $TEMPDIR/ocpwin64/.
   rsync -auv ../ocamlpro/. $TEMPDIR/ocamlpro/.
   rsync -auv ../ocpwin/. $TEMPDIR/ocpwin/.
   
   mkdir -p $TEMPDIR/ocp-build-watcher
   rsync -auv ../ocp-build-watcher/. $TEMPDIR/ocp-build-watcher/.
   
   git rev-parse --short HEAD > COMMIT.release
   cp COMMIT.release $TEMPDIR/ocpwin32/byterun/
   cp COMMIT.release $TEMPDIR/ocpwin32/cross/byterun/
   cp COMMIT.release $TEMPDIR/ocpwin64/byterun/
   cp COMMIT.release $TEMPDIR/ocpwin64/cross/byterun/
   
   export OCPWATCHER=0
   (cd $TEMPDIR; make) &> $TEMPDIR.log &
   
done

tail -f /tmp/ocpwin-release-*.log


