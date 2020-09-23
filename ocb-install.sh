#!/bin/bash

set -eu

. ./defs.sh

if [ "$#" -ne 1 ]
then
  readonly this="${0}"

  echo "Usage: ${this} prefix"
  echo
  echo "For example '${this} /home/john/.local' will copy:"
  echo "  * executables into    /home/john/.local/bin"
  echo "  * library files into  /home/john/.local/share/herdtools7"
  exit 1
fi

readonly prefix="${1}"

readonly bindir="${prefix}/bin"
readonly libdir="${prefix}/share/herdtools7"

mkdir -p "${bindir}"
mkdir -p "${libdir}"

cpbin () {
  local sub="${1}"
  local execs="${2}"
  for exec in $execs
  do
    cp "_build/${sub}/${exec}" "${bindir}/$(basename $exec .native)7"
  done
}

# Copy binaries
cpbin herd "$HERD"
cpbin litmus "$LITMUS"
cpbin tools "$TOOLS"
cpbin gen "$GEN"
cpbin jingle "$JINGLE"

# Copy libfiles
cpdir herd/libdir   "${libdir}/herd"
cpdir litmus/libdir "${libdir}/litmus"
cpdir jingle/libdir "${libdir}/jingle"
