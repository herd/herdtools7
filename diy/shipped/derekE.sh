set -e
DIR=`dirname $0`
LITMUSOPTS="-mach power7 -mach power7.smt -kind false -s 5k -r 2k -mem direct -st 1"
litmus $LITMUSOPTS derekE-src/@all -conds derekE-src/conds.txt -o derekE-huge.tgz

