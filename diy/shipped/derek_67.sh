set -e
DIR=`dirname $0`
LITMUSOPTS="-signaling true"
export LITMUSOPTS
. $DIR/funs.sh
zyva ../../mem.new/tests/derek/MP+lwsyncs+signal.litmus > derek6.tgz
zyva ../../mem.new/tests/derek/WRC+lwsync+addr+signal.litmus > derek7.tgz