Use litmus7 to generate code from a litmus test

  $ TEST="A009"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../herd/tests/instructions/X86_64/$TEST.litmus"  \
  > -mach x86_64 -mode std -a 2 -s 1k -r 100


Compile and run the litmus test natively, avoid printing the timing, it's not
stable

  $ cd $TEST
  $ make > /dev/null
  $ "./$TEST.exe" | grep -v -e ^Time
  Test A009 Required
  Histogram (1 states)
  200000:>0:rcx=-1;
  Ok
  
  Witnesses
  Positive: 200000, Negative: 0
  Condition forall (0:rcx=-1) is validated
  Hash=7ca3c35015d75a877ccf509d75062e79
  Observation A009 Always 200000 0

