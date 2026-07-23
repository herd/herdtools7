Use litmus7 to generate code from a litmus test

  $ TEST="A009"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../herd/tests/instructions/X86_64/$TEST.litmus"  \
  > -mode std -a 4 -s 1k -r 100 \
  > -mach x86_64

Compile and run the litmus test natively, avoid printing the timing, it's not
stable

  $ cd $TEST
  $ make > /dev/null
  $ "./$TEST.exe" | sed '$d'
  Test A009 Required
  Histogram (1 states)
  4000000:>0:rcx=-1;
  Ok
  
  Witnesses
  Positive: 4000000, Negative: 0
  Condition forall (0:rcx=-1) is validated
  Hash=7ca3c35015d75a877ccf509d75062e79
  Observation A009 Always 4000000 0

