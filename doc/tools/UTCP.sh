#!/bin/sh

set -eu

herd7 -model pretty.cat -debug model -skipchecks uniproc,thinair,observation,propagation $*
