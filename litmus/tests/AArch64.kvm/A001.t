Use litmus7 to generate code from a litmus test

  $ TEST="A001"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > -mode std -a 4 -s 1k -r 100 \
  > "../../../herd/tests/instructions/AArch64.kvm/$TEST.litmus"  \
  > -mach aarch64

Compile and run the litmus test natively, avoid printing the timing, it's not
stable

  $ cd "$TEST"
  $ make > /dev/null
  $ "./$TEST.exe" | sed '$d'
  Test A001 Required
  Histogram (1 states)
  4000000*>0:X0=1; 0:X2=1;
  No
  
  Witnesses
  Positive: 0, Negative: 4000000
  Condition forall (0:X0=0 /\ 0:X2=0) is NOT validated
  Hash=b4f76ddaf0ec77a089ddb069cb87815f
  Variant=fatal
  Observation A001 Never 0 4000000
