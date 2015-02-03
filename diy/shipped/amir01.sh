SRC=~/Chianti/sem/WeakMemory/ppcmem/results/paper/@all
TMP=/tmp/amir.$$
mkdir -p $TMP
D0=amir00
mkdir $TMP/$D0
mprog -transpose true -o $TMP/$D0 $SRC
mapply -j 16 -com herd $SRC  > $TMP/Model
mlog2cond -forall true -optcond true  $TMP/Model > $TMP/conds.txt
mkdir $TMP/src
recond -conds $TMP/conds.txt -o $TMP/src $SRC
D1=amir01
mkdir $TMP/$D1
mprog -transpose true $TMP/src/@all -o $TMP/$D1
cp $TMP/src/@all $TMP/$D1
cp $TMP/src/@all $TMP/$D0
( cd $TMP && tar zcf - $D0 ) > $D0.tgz
( cd $TMP && tar zcf - $D1 ) > $D1.tgz
 /bin/rm -rf $TMP
