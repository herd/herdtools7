set -e
DIR=`dirname $0`
LITMUSOPTS="-mach power7.smt -kind false -r 100"
. $DIR/new_funs.sh
zyva ../../mem.new/tests/VAR4/@all > derekEIEIO.tgz
