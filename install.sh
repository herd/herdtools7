#!/bin/bash
DIR=$(dirname $0)
. $DIR/defs.sh

if [ "x$1" = "x" ]
then
  echo "Usage: ./install.sh prefix"
  echo
  echo "For example './install.sh /home/john/.local' will copy:"
  echo "  * executables into    /home/john/.local/bin"
  echo "  * library files into  /home/john/.local/share/herdtools7"
  exit 1
else
  PREFIX=$1
fi

BINDIR=$PREFIX/bin
LIBDIR=$PREFIX/share/herdtools7

cpbin () {
  SUB=$1
  EXECS="$2"
  for exec in $EXECS
  do
    cp $DIR/_build/$SUB/$exec $BINDIR/$(basename $exec .native)7
  done
}

#copy binaries
cpbin herd "$HERD"
cpbin litmus "$LITMUS"
cpbin tools "$TOOLS"
cpbin gen "$GEN"
cpbin jingle "$JINGLE"

#copy libfiles
cpdir herd/libdir $LIBDIR/herd
cpdir litmus/libdir $LIBDIR/litmus
cpdir jingle/libdir $LIBDIR/jingle
