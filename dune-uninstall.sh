#!/bin/bash

set -eu

if [ "$#" -ne 1 ]
then
  readonly this="${0}"

  echo "Usage: ${this} <prefix>"
  echo
  echo "For example '${this} /home/john/.local' will remove:"
  echo "  * executables from      /home/john/.local/bin"
  echo "  * delete the directory  /home/john/.local/share/herdtools7"
  exit 1
fi

readonly prefix="${1}"

readonly bindir="${prefix}/bin"
readonly libdir="${prefix}/share/herdtools7"

# Print out the commands that this script runs.
set -x

# Remove binaries
dune uninstall --prefix "${prefix}"

# Remove libfiles
rm -rf "${libdir}"
