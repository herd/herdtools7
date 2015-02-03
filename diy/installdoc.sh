set -e
DIR=`dirname 0`
INSTALLDIR=$1
make -C $DIR/doc  INSTALLDIR=$INSTALLDIR all install
( cd $INSTALLDIR && tar xf doc/x86.tar && mv x86 example )