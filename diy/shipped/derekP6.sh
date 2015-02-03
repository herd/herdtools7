set -e
DIR=`dirname $0`
LITMUSOPTS="-mach power6.smt -kind false -s 100k -r 100"
. $DIR/new_funs.sh
zyva ../../mem.new/tests/VARC/@derekP6 > derekP6.tgz
