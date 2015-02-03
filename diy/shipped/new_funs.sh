# Shipped to derek, four archs
LITMUS="litmus"
UNIQ=/tmp/ship.$$
TMP=$UNIQ/derek
zyva () {
TESTS="$@"
mkdir -p $TMP
$LITMUS $TESTS -mach power7.32 $LITMUSOPTS -cross $TMP/linux.32.tar
( cd $TMP && tar xf linux.32.tar && mv litmus_tests linux.32 )
$LITMUS $TESTS -mach power7.32 -ws w64 $LITMUSOPTS -cross $TMP/linux.64.tar
( cd $TMP && tar xf linux.64.tar && mv litmus_tests linux.64 )
$LITMUS $TESTS -mach power7.32 -os aix $LITMUSOPTS -cross $TMP/aix.32.tar
( cd $TMP && tar xf aix.32.tar && mv litmus_tests aix.32 )
$LITMUS $TESTS -mach power7.32 -ws w64 -os aix $LITMUSOPTS -cross $TMP/aix.64.tar
( cd $TMP && tar xf aix.64.tar && mv litmus_tests aix.64 )
( cd $TMP && /bin/rm *.tar && tar czf - . )
/bin/rm -rf $UNIQ
}