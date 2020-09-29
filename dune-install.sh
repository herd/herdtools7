#!/bin/bash

set -eu

if [ "$#" -ne 1 ]
then
  readonly this="${0}"

  echo "Usage: ${this} <prefix>"
  echo
  echo "For example '${this} /home/john/.local' will copy:"
  echo "  * executables into    /home/john/.local/bin"
  echo "  * library files into  /home/john/.local/share/herdtools7"
  exit 1
fi

readonly prefix="${1}"
readonly libdir="${prefix}/share/herdtools7"

. ./defs.sh

# Copy binaries
dune install --prefix "${prefix}"

# Copy libfiles
cpdir herd/libdir   "${libdir}/herd"
cpdir litmus/libdir "${libdir}/litmus"
cpdir jingle/libdir "${libdir}/jingle"
