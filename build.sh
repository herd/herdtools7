#!/bin/bash

set -o errexit

OCAMLBUILD="ocamlbuild -no-links -j 5"

VERSION=$(grep "^version:" opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
LIBDIR=$1
if [ "x$LIBDIR" = "x" ]
then
  LIBDIR=$(pwd)/libdir
fi

rm -f */version.ml

cd herd
cat > version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/herd"
EOD
$OCAMLBUILD herd.native
cd ..

cd gen
cat > version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/gen"
EOD
$OCAMLBUILD atomize.native atoms.native classify.native diy.native diycross.native diyone.native dont.native mexpand.native nexts.native readRelax.native
cd ..

cd litmus
cat > version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/litmus"
EOD
$OCAMLBUILD litmus.native
cd ..

cd tools
cat > version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/tools"
EOD
$OCAMLBUILD knames.native ksort.native madd.native mapply.native mcmp.native mcompare.native mcond.native mcycles.native mdiag.native mdiff.native mfilter.native mfind.native mflags.native mhash.native mlog2cond.native mmixer.native mnames.native mobserved.native moutcomes.native mprog.native mproj.native mrcu.native mselect.native mshowhashes.native msort.native msum.native mtopos.native recond.native rehash.native splitcond.native splitdot.native
cd ..

cd jingle
cat > version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/jingle"
EOD
$OCAMLBUILD jingle.native
cd ..
