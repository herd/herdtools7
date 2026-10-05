Use litmus7 to generate code from a litmus test

  $ TEST="A014"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "../../../herd/tests/instructions/X86_64/$TEST.litmus"  \
  > -mach x86_64 -mode presi -a 2 -s 1k -r 100 -alloc static


Compile and run the litmus test natively, avoid printing the timing, it's not
stable

  $ cd $TEST
  $ make > /dev/null
  $ "./$TEST.exe" | grep ^Observation
  Observation A014 Never 0 100000

