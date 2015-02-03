IDX=$1
LOG=$2
NAME=$3
TMP=/tmp/restrict.$$
mkdir -p $TMP
safe -o $TMP/MIN mapply -j 16 -com herd -comargs -model,minimal $IDX
mcompare $TMP/MIN $LOG -neg $TMP/N -cneg $NAME.txt -pos $TMP/P -optcond true >/dev/null
mfind -names $TMP/N $IDX > @$NAME
echo '**CHECK'
cat $TMP/P
echo '**END'
/bin/rm -rf $TMP
