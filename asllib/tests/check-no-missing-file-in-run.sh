#!/bin/bash

# This script execute [check_no_missing_file_in_run] for each [run.t] file in
# [asllib/tests].
#
# The executable [check_no_missing_file_in_run] is recompiled with [dune].

# bash configuration
set -o errexit    # Used to exit upon error, avoiding cascading errors
set -o nounset    # Exposes unset variables

# Our executable
checker="$1"/_build/default/asllib/tests/check_no_missing_file_in_run.exe

# Recompilation if needed
dune build --profile=release ${checker}

for d in "$1"/asllib/tests/*.t/; do
  ${checker} ${d} $(git ls-files HEAD ${d}) &
done

wait

