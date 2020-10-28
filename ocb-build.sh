#!/bin/bash

set -eu

. ./defs.sh

if [ "$#" -eq 1 ]
then
	readonly libdir="${1}/share/herdtools7"
else
	readonly libdir="$(pwd)/_build/libdir"
	rm -rf "${libdir}"
	mkdir -p "${libdir}"
	cpdir herd/libdir   "${libdir}/herd"
	cpdir litmus/libdir "${libdir}/litmus"
	cpdir jingle/libdir "${libdir}/jingle"
fi

cat > Version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$libdir/"
EOD

ocamlbuild -no-links -j 5 -cflags -w,+a-3-4-9-29-33-41-45-67,-strict-sequence ${NATIVE} ${INTERNAL} ${TESTS}
# Warnings ignored on purpose:
# Warning 4: this pattern-matching is fragile
# Warning 33: unused open
# Warning 45: this open statement shadows the constructor
# Warning 67: unused functor parameter

# Warnings ignored temporarily to allow warning-free compilation (@todo Re-enable):
# Warning 9: the following labels are not bound in this record pattern
# Warning 27: unused variable
# Warning 29: unescaped end-of-line in a string constant
# Warning 41: xxx belongs to several types
