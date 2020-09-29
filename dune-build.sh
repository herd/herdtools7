#!/bin/sh

set -eu

if [ "$#" -ne 1 ]
then
  readonly this="${0}"

  echo "Usage: ${this} <prefix>"
  exit 1
fi

readonly prefix="${1}"

# Print out the commands that this script runs.
set -x

./version-gen.sh "${prefix}"

dune build --profile release
