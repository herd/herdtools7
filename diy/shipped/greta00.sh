#First pack of 59 tests for Greta Yorsh (CortexA15)
set -e
DIR=`dirname $0`
LITMUSOPTS="-kind false -mach cortexA15"
IDX=../../mem.new/tests/@A15
TAG=greta00
TMP=/tmp/dir.$$
mkdir -p $TMP/$TAG
litmus $LITMUSOPTS -o $TMP/$TAG $IDX
(cd $TMP && tar zcf $TAG.tgz $TAG )
mv $TMP/$TAG.tgz .
/bin/rm -rf $TMP

