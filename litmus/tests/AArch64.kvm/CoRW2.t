Test how the faults are printed in the histogram

  $ TEST="memtag-CoRW2+rmw+T"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../gen/tests/AArch64.MTE/$TEST.litmus" \
  > -mach kvm-armv8.5-a+memtag \
  > -a 4 -s 10 -r 1

Run the test, and strip the number of occurrences from the log, to make the
output stable.

  $ cd "$TEST"
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=30s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -E 's/^[0-9]+[[:blank:]]+:>/:>/' > run.log

Show all aliases of the faulting instruction, including Loop00, which is not
named in the condition.
Herd also permits a TagCheck fault at L01 (STXR), with no fault at L00.
This outcome was not observed while developing the test, so we filter it out to
make sure it doesn't change the test result

  $ grep '^:>' run.log | grep -Fvx ':>0:X1=0; fault(P0:L01,x,TagCheck); ~fault(P0:L00,x);' | sort
  :>0:X1=0; fault(P0:{L00,Loop00},x,TagCheck); ~fault(P0:L01,x);
  :>0:X1=0; ~fault(P0:L00,x); ~fault(P0:L01,x);


  $ grep '^Condition exists' run.log
  Condition exists (0:X1=1 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x))) is NOT validated
