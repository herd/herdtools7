# Shipped to derek, for nolwsync machines, augmented bucket.
LITMUS="litmus -speedcheck true"
NAME=nolwsync2
TESTS=../../mem.new/tests/@nolwsync2
UNIQ=/tmp/ship.$$
TMP=$UNIQ/$NAME
mkdir -p $TMP
$LITMUS $TESTS -mach power7.32 -cross $TMP/linux.32.tar
( cd $TMP && tar xf linux.32.tar && mv litmus_tests linux.32 )
$LITMUS $TESTS -mach power7.32 -ws w64 -cross $TMP/linux.64.tar
( cd $TMP && tar xf linux.64.tar && mv litmus_tests linux.64 )
( cd $TMP && /bin/rm *.tar && tar czf - . ) > $NAME.tgz
/bin/rm -rf $UNIQ
