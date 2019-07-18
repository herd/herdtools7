TGT=run-all
/bin/rm -rf $TGT
SCRIPT=./ALL.sh
mkdir -p $TGT
for m in M1 M2 M3
do
  echo '**' $m
  MDL=$(echo $m | tr A-Z a-z).cat
  safe -o $TGT/$m mapply7 -j 16 -com $SCRIPT -comargs -model,$MDL $@
done > $TGT/LOG

