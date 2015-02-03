set -e
DIR=`dirname $0`
LITMUSOPTS="-signaling true"
export LITMUSOPTS
. $DIR/funs.sh
zyva ../../mem.new/tests/derek/WRC+lwsync-loop+addr+signal.litmus > derek8.tgz
