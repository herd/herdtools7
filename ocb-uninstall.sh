#!/bin/bash

. ./defs.sh

if [ "x$1" = "x" ]
then
  echo "Usage: ./uninstall.sh prefix"
  echo
  echo "For example './install.sh /home/john/.local' will remove:"
  echo "  * executables from      /home/john/.local/bin"
  echo "  * delete the directory  /home/john/.local/share/herdtools7"
  exit 1
else
  PREFIX=$1
fi

BINDIR=$PREFIX/bin
LIBDIR=$PREFIX/share/herdtools7

rmbin () {
  EXECS="$1"
  for exec in $EXECS
  do
    rm -f $BINDIR/$(basename $exec .native)7
  done
}

# Remove binaries
rmbin "$NATIVE"

# Remove libfiles
rm -rf $LIBDIR
