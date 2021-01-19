#!/bin/sh

set -eu

if [ "$#" -ne 1 ]
then
  readonly this="${0}"

  echo "Usage: ${this} prefix"
  echo
  echo "For example '${this} /home/john/.local' will remove:"
  echo "  * executables from      /home/john/.local/bin"
  echo "  * delete the directory  /home/john/.local/share/herdtools7"
  exit 1
fi

readonly prefix="${1}"

. ./defs.sh

readonly bindir="${prefix}/bin"
readonly libdir="${prefix}/share/herdtools7"

rmbin () {
  execs="$1"
  for exec in $execs
  do
    rm -f "${bindir}/$(basename $exec .native)7"
  done
}

# Remove binaries
rmbin "$NATIVE"

# Remove libfiles
rm -rf "${libdir}"
