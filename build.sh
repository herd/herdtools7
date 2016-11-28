#!/bin/bash
#Assume run as ./build.sh
. ./defs.sh

if [ "x$1" = "x" ]
then
  LIBDIR=$(pwd)/_build/libdir
  rm -rf $LIBDIR
  mkdir -p $LIBDIR
  cpdir herd/libdir $LIBDIR/herd
  cpdir litmus/libdir $LIBDIR/litmus
  cpdir jingle/libdir $LIBDIR/jingle
else
  LIBDIR=$1/share/herdtools7
fi

cat > Version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/"
EOD

ocamlbuild -no-links -j 5 -cflags -w,+a-4-9-27-29-33-41-45,-strict-sequence $NATIVE
# Warnings ignored on purpose:
# Warning 4: this pattern-matching is fragile
# Warning 33: unused open
# Warning 45: this open statement shadows the constructor

# Warnings ignored temporarily to allow warning-free compilation (@todo Re-enable):
# Warning 9: the following labels are not bound in this record pattern
# Warning 27: unused variable
# Warning 29: unescaped end-of-line in a string constant
# Warning 41: xxx belongs to several types
