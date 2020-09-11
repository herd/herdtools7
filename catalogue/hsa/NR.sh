#!/bin/sh

set -eu

herd7 -conf luc.cfg -bell bells/luc.bell -badexecs false -badflag undefined $@
