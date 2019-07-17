#!/bin/bash

. ./dune-defs.sh

if [ "x$1" = "x" ]
then
  echo "Usage: ./dune-install.sh prefix"
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

if ! [ -d $BINDIR ]
then
	mkdir -p $BINDIR
fi

if ! [ -d $LIBDIR ]
then
	mkdir -p $LIBDIR
fi

cpbin () {
  for exec
  do
    cp _build/default/$exec $BINDIR/$(basename $exec .exe)7
  done
}

# Copy binaries
#cpbin $EXE
dune install --prefix $PREFIX
# Copy libfiles
cpdir herd/libdir $LIBDIR/herd
cpdir litmus/libdir $LIBDIR/litmus
cpdir jingle/libdir $LIBDIR/jingle
