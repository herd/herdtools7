# Shipped to derek for reasons he does not want to tell..
LITMUS=litmus
TESTS=../../mem.new/tests/VAR2/MP/MP+lwsync+addr.litmus
UNIQ=/tmp/ship.$$
TMP=$UNIQ/derek3
mkdir -p $TMP
$LITMUS $TESTS -mach power7.32 -cross $TMP/linux.32.tar
( cd $TMP && tar xf linux.32.tar && mv litmus_tests linux.32 )
$LITMUS $TESTS -mach power7.32 -ws w64 -cross $TMP/linux.64.tar
( cd $TMP && tar xf linux.64.tar &&  mv litmus_tests linux.64 )
$LITMUS $TESTS -mach power7.32 -os aix -cross $TMP/aix.32.tar
( cd $TMP && tar xf aix.32.tar  && mv litmus_tests aix.32 )
$LITMUS $TESTS -mach power7.32 -ws w64 -os aix -cross $TMP/aix.64.tar
( cd $TMP && tar xf aix.64.tar && mv litmus_tests aix.64 )
( cd $TMP && ( find . -name '*.tar' | xargs /bin/rm ) && tar czf - . ) > derek3.tgz
/bin/rm -rf $UNIQ
