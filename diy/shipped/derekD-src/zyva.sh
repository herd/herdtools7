#12 sec per test X 623 test X 6 runs -> 12.5 hours
OPT="-s 5M -r 2 +ca"
TGT=SMALL
sh run.sh $OPT -st 133 > $TGT.00
sh run.sh $OPT -st 1 > $TGT.01
sh run.sh $OPT -st 2 > $TGT.02
sh run.sh $OPT -st 3 > $TGT.03
sh run.sh $OPT -st 4 > $TGT.04
sh run.sh $OPT -st 7 > $TGT.07
