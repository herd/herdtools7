#!/bin/bash
DIR=$(dirname $0)
. $DIR/defs.sh
set -o verbose
set -o errexit

VERSION=$(grep "^version:" opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
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

ocamlbuild -no-links -j 5 $NATIVE
