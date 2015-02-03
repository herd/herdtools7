set -e
DIR=`dirname $0`
LITMUSOPTS="-kind false -s 1k -r 100k -randomise_affinity true"
. $DIR/new_funs.sh
zyva ../../mem.new/tests/IRIW+addrs-twice.litmus > derekA.tgz
