#! /bin/sh
DIR=$(dirname $0)
CMD=$DIR/pp2ml.byte
ARG=$1
DARG=$(dirname $ARG)
BOUT=$(basename $ARG .tpl)
OUT=$DARG/$BOUT
exec $CMD $ARG > $OUT