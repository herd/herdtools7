set -e
DIR=`dirname $0`
. $DIR/funs.sh
zyva ../../mem.new/tests/derek/MP+lwsync+lwsync-loop.litmus > derek4.tgz
zyva ../../mem.new/tests/derek/WRC+lwsync-loop+addr-loop.litmus > derek5.tgz