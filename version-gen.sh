#!/bin/sh

set -eu

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <project-root>" >&2
  exit 1
fi

cd "$1"

. ./defs.sh

cat > Version.ml <<EOF
(* GENERATED, DO NOT EDIT *)
let version = "$VERSION"
let rev = "$REV"
EOF
