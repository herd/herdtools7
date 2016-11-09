#!/bin/bash

set -o errexit

if [ "x$1" = "x" ]
then
  echo "Usage: ./install_without_opam prefix"
  echo
  echo "For example './install_without_opam /home/john/.local' will put executables in /home/john/.local/bin"
  exit 1
else
  PREFIX=$1
fi

BINDIR=$PREFIX/bin
LIBDIR=$PREFIX/share/herdtools7

./build.sh $LIBDIR

mkdir -p $BINDIR
for EXECUTABLE in _build/*/*.native
do
  STEM=$(basename $EXECUTABLE .native)
  cp $EXECUTABLE $BINDIR/${STEM}7
done

rm -rf $LIBDIR
mkdir -p $LIBDIR
cp -r herd/libdir $LIBDIR/herd
cp -r litmus/libdir $LIBDIR/litmus
cp -r jingle/libdir $LIBDIR/jingle
