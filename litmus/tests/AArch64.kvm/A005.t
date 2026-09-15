Use litmus7 to generate code from a litmus test

  $ TEST="A005"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../herd/tests/instructions/AArch64.kvm/$TEST.litmus"  \
  > -mode std -a 4 -s 1k -r 100 \
  > -mach aarch64

Compile and run the litmus test natively, avoid printing the timing, it's not
stable

  $ cd $TEST
  $ make > /dev/null
  $ "./$TEST.exe" | sed '$d'
  Test A005 Required
  Histogram (1 states)
  4000000*>0:X0=1; 0:X2=0; 0:X3=1;
  No
  
  Witnesses
  Positive: 0, Negative: 4000000
  Condition forall (0:X0=0 /\ 0:X2=1 /\ 0:X3=1) is NOT validated
  Hash=75f5df758b5a4cc324cb2f4cb763edd5
  Observation A005 Never 0 4000000
