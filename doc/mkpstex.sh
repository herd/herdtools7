#! /bin/sh -
BASE=$1
FONTSZ=$2
COM=$3
OPTS="$4"
$COM $OPTS -Tfig $BASE.dot > $BASE.fig
fig2dev -L pstex $BASE.fig $BASE.pstex
fig2dev -L pstex_t -p $BASE.pstex -s $FONTSZ $BASE.fig |\
sed -e 's@generated/@@' > $BASE.pstex_t
