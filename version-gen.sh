#! /bin/sh
. ./dune-defs.sh
LIBDIR=$1/share/herdtools7
OUT=$2
cat > Version.ml <<EOF
(* GENERATED, DO NOT EDIT *)
let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/"
EOF
