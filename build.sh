#!/bin/bash

set -o verbose
set -o errexit

OCAMLBUILD="ocamlbuild -no-links -j 5"

VERSION=$(grep "^version:" opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
if [ "x$1" = "x" ]
then
  LIBDIR=$(pwd)/libdir
else
  LIBDIR=$1
fi

cat > lib/Version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/"
EOD


cd herd
$OCAMLBUILD herd.native
cd ..

cd gen
$OCAMLBUILD atomize.native atoms.native classify.native diy.native diycross.native diyone.native dont.native mexpand.native nexts.native readRelax.native
cd ..

cd litmus
$OCAMLBUILD litmus.native
cd ..

cd tools
$OCAMLBUILD knames.native ksort.native madd.native mapply.native mcmp.native mcompare.native mcond.native mcycles.native mdiag.native mdiff.native mfilter.native mfind.native mflags.native mhash.native mlog2cond.native mmixer.native mnames.native mobserved.native moutcomes.native mprog.native mproj.native mrcu.native mselect.native mshowhashes.native msort.native msum.native mtopos.native recond.native rehash.native splitcond.native splitdot.native
cd ..

cd jingle
$OCAMLBUILD jingle.native
cd ..
