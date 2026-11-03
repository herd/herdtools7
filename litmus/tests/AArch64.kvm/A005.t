Use litmus7 to generate code from a litmus test

  $ TEST="A005"
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
  Test A005 Required
  Histogram (1 states)
  40    :>0:X0=0; 0:X2=1; 0:X3=1;
  Ok
  Witnesses
  Positive: 40, Negative: 0
  Condition forall (0:X0=0 /\ 0:X2=1 /\ 0:X3=1) is validated
  Hash=75f5df758b5a4cc324cb2f4cb763edd5
  Observation A005 Always 40 0
  
  

