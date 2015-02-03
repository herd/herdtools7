set -e
DIR=`dirname $0`
LITMUSOPTS="-kind false -r 100"
. $DIR/new_funs.sh
zyva ../../mem.new/@derek9 > derek9.tgz
