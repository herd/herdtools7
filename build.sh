#!/bin/bash

set -o verbose
set -o errexit

VERSION=$(grep "^version:" opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
if [ "x$1" = "x" ]
then
  LIBDIR=$(pwd)/_build/libdir
  rm -rf $LIBDIR
  mkdir -p $LIBDIR
  cp -r herd/libdir $LIBDIR/herd
  cp -r litmus/libdir $LIBDIR/litmus
  cp -r jingle/libdir $LIBDIR/jingle
else
  LIBDIR=$1
fi

cat > Version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/"
EOD

ocamlbuild -no-links -j 5 \
herd.native atomize.native atoms.native classify.native diy.native diycross.native diyone.native \
dont.native mexpand.native nexts.native readRelax.native litmus.native knames.native ksort.native \
madd.native mapply.native mcmp.native mcompare.native mcond.native mcycles.native mdiag.native \
mdiff.native mfilter.native mfind.native mflags.native mhash.native mlog2cond.native mmixer.native \
mnames.native mobserved.native moutcomes.native mprog.native mproj.native mrcu.native \
mselect.native mshowhashes.native msort.native msum.native mtopos.native recond.native \
rehash.native splitcond.native splitdot.native jingle.native
