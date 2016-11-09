#!/bin/bash

set -o errexit

# Debug, tests, coverage
# ======================

# @todo Build with bisect_ppx, run unit test, generate test coverage reports, etc.
./build.sh

# OPAM package
# ============

opam pin --yes --no-action add .
opam reinstall --yes herdtools7

echo
echo "Development cycle OK"
