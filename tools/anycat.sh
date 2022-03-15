# Generating appropriate exists clause for any cat

J=8
config="X.conf"
testcat=./A.cat
strictcat=./C.cat

diyoptions=
strictoptions=

rm -f src/*.litmus
( cd src && diy7 $diyoptions -conf $config )
mapply7 -j $J -com herd7 -comargs -cat,$testcat src/@all > S
grep Obs S | grep  Never | awk '{print $2}' > src/Forbid
(cd src && mfind7 -names Forbid @all > @forbid )
mapply7 -j $J -com herd7 -comargs $strictoptions-cat,$strictcat src/@forbid > C

#### All violations
mcompare7 -optcond true -pos P -cpos p.txt S C >/dev/null
rm -rf OUT2 && mkdir OUT2
recond7 -names P -conds p.txt -o OUT2 src/@all
mv p.txt OUT2/conds.txt

####
rm -f P
