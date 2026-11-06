Use litmus7 to generate code from a litmus test

  $ TEST="A013"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../herd/tests/instructions/AArch64.kvm/$TEST.litmus"  \
  > -mach kvm-armv8.5-a+memtag \
  > -a 4 -s 10 -r 1

Compile and run the litmus test under QEMU, avoid printing the timing, it's not
stable

  $ cd "$TEST"
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=30s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -n '/^EXIT:/q; /^Test /,$p' | grep -v '^Time'
  Test A013 Required
  Histogram (1 states)
  40    :> fault(P0:L0,UndefinedInstruction); fault(P0:L1,x,D-MMU:Translation); fault(P0:L2,y,D-MMU:AccessFlag);
  Ok
  Witnesses
  Positive: 40, Negative: 0
  Condition forall (fault(P0)) is validated
  Hash=3bb0ea14af4dd60a39e415c543a44f81
  Variant=faultToNext
  Observation A013 Always 40 0
  Faults A013 120 P0:120
  
  
