#Big pack of tests for Mike Dobs (ARMv6K)
set -e
DIR=`dirname $0`
LITMUSOPTS="-mach mike.cfg"
IDX=../../mem.new/tests/@ARM
TAG=mike00
TMP=/tmp/dir.$$
mkdir -p $TMP/$TAG
litmus $LITMUSOPTS -o $TMP/$TAG $IDX
(cd $TMP && tar zcf $TAG.tgz $TAG )
mv $TMP/$TAG.tgz .
/bin/rm -rf $TMP

