Use litmus7 to generate code from a litmus test

  $ TEST="A04"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../herd/tests/instructions/AArch64/$TEST.litmus"  \
  > -mode std -a 4 -s 1k -r 100 \
  > -mach aarch64

Compile and run the litmus test natively, avoid printing the timing, it's not
stable

  $ cd "$TEST"
  $ make > /dev/null
  $ "./$TEST.exe" | sed '$d'
  Test A4 Allowed
  Histogram (1 states)
  4000000*>0:X0=0;
  Ok
  
  Witnesses
  Positive: 4000000, Negative: 0
  Condition exists (0:X0=0) is validated
  Hash=d64e911ddfaa8df8f0c4d17557b7562d
  Observation A4 Always 4000000 0
