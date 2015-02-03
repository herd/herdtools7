set -e
DIR=`dirname $0`
LITMUSOPTS="-mach power7.smt -kind false -s 100k -r 100"
. $DIR/new_funs.sh
zyva ../../mem.new/tests/VARC/@derekP7 > derekP7.tgz
