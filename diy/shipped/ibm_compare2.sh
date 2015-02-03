set -e
DIR=`dirname $0`
TMP=/var/tmp
EXPORT=$TMP/exp.$$
NAME=ibm_compare2
DNAME=$EXPORT/$NAME
mkdir $EXPORT && mkdir $DNAME
#ship mcompare
sh $DIR/../ibm_compare.sh
mv ./ibm_compare.tar.gz $DNAME
( cd $DNAME && tar xzf ./ibm_compare.tar.gz && /bin/rm ./ibm_compare.tar.gz && mv ibm_compare mcompare )
#toplevel Makefile
(
cat <<'EOF'
COMPARE = ./mcompare/compare.byte

MODELS= reference_data/model-CAV

ARCHS= reference_data/Power6 reference_data/PowerG5


go: $(MODELS) $(ARCHS) Makefile  $(COMPARE) 
	$(COMPARE) -nomodel -rename reference_data/name_map.txt -kinds reference_data/power_arch_intention.txt -show X  $(MODELS) Power7 $(ARCHS) 

 $(COMPARE):
	$(MAKE) -C `dirname $(COMPARE)` `basename $(COMPARE)`

EOF
) > $DNAME/Makefile

#toplevel Makefile
(
cat <<'EOF'
`date`
1) run tests to produce a log file, as described in the test tarball

2) copy that logfile to a file ibm_compare2/Power7, overwriting the 
   one that's there (produced on our power7).

3) "make go" in ibm_compare/  (which (1) build the log-analysis
    tool in sub-directoty 'mcompare' and then runs it so as to 
    produce a table on stdout)
EOF
) > $DNAME/README
#Now copy name maps & kinds for @test3_run
TOP=$DIR/../..
MEM=$TOP/mem.new
TESTS=$MEM/@test3_run
RFD=$DNAME/reference_data
mkdir $RFD
cp $MEM/paper_kinds.txt $RFD/power_arch_intention.txt
cp $MEM/name_map_paper.txt  $RFD/name_map.txt
#Model
mkdir generated
memevents $TESTS > $RFD/model-CAV
/bin/rm -rf generated
#Power6/PowerG5 data
DATA1=$TOP/litmus.new/results_ppc2/paper
( cd $DATA && sh paper.sh )
DATA2=$TOP/litmus.new/results_ppc2
make -C $DATA2  realclean PowerG5 Power6
$MEM/sum $DATA1/PowerG5 $DATA2/PowerG5 > $RFD/PowerG5
$MEM/sum $DATA1/Power6 $DATA2/Power6 > $RFD/Power6
$MEM/sum $DIR/Power7.32 $DIR/Power7.64 > $DNAME/Power7
( cd $EXPORT && tar zcf - $NAME ) > $NAME.tgz
/bin/rm -rf $EXPORT

