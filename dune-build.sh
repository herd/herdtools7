. ./dune-defs.sh

LIBDIR=$1/share/herdtools7
cat > Version.ml <<EOD
(* GENERATED, DO NOT EDIT *)

let version = "$VERSION"
let rev = "$REV"
let libdir = "$LIBDIR/"
EOD

dune build $EXE

