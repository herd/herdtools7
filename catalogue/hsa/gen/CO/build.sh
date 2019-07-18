/bin/rm -rf TMP
mkdir -p TMP
diy7 -conf X.conf -o TMP
herd7 -model uniproc.cat TMP/@all > TMP/U
mlog2cond7 -optcond true -forall true TMP/U > TMP/cond.txt
recond7 -conds TMP/cond.txt TMP/@all -o .
/bin/rm -rf TMP
