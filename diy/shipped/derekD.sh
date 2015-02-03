set -e
DIR=`dirname $0`
LITMUSOPTS="-mach power7 -mach power7.smt -kind false -s 5k -r 2k"
litmus $LITMUSOPTS derekD-src/@small -conds derekD-src/small.txt -o derekD-small.tgz
litmus $LITMUSOPTS derekD-src/@huge -conds derekD-src/huge.txt -o derekD-huge.tgz

