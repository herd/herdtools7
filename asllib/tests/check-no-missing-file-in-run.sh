#!/bin/bash

set -o errexit    # Used to exit upon error, avoiding cascading errors
set -o nounset    # Exposes unset variables

counter=0

for d in "$1"/asllib/tests/*.t/; do
  for f in $(git ls-files HEAD "$d*.asl"); do
    if ! grep -q "${f##*/}" "${d}run.t"; then
      echo "ASL file committed and not run: $f";
      counter=$counter+1;
    fi;
  done
done

if [[ $counter -gt 0 ]]; then
  exit 1;
else
  exit 0;
fi;
