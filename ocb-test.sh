#!/bin/sh

set -eu

. ./defs.sh

for test in ${TESTS}
do
	"$(find ./_build -name "${test}")"
done
