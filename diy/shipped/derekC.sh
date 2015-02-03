set -e
DIR=`dirname $0`
LITMUSOPTS="-mach power7 -kind false -s 100k -r 100"
. $DIR/new_funs.sh
zyva /home/chianti/maranget/sem/WeakMemory/mem.new/@derekC > derekC.tgz
