#!/bin/sh

set -eu

if [ "$#" -ne 1 ]
then
  readonly this="${0}"

  echo "Usage: ${this} <prefix>"
  exit 1
fi

readonly libdir="${1}/share/herdtools7"

. ./defs-mini.sh

cat > Version.ml <<EOF
(* GENERATED, DO NOT EDIT *)
let version = "$VERSION"
let rev = "$REV"
let libdir = "$libdir/"
EOF
