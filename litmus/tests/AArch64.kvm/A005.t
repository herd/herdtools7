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
  A005.c:305:83: warning: value size does not match register size specified by the constraint and modifier [-Wasm-operand-widths]
    305 | :[x3] "=&r" (*out_0_x3),[x2] "=&r" (*out_0_x2),[x0] "=&r" (*out_0_x0),[x4] "=&r" (trashed_x4)
        |                                                                                   ^
  A005.c:297:6: note: use constraint modifier "w"
    297 | "adr %[x4],0b\n"
        |      ^~~~~
        |      %w[x4]
  A005.c:305:83: warning: value size does not match register size specified by the constraint and modifier [-Wasm-operand-widths]
    305 | :[x3] "=&r" (*out_0_x3),[x2] "=&r" (*out_0_x2),[x0] "=&r" (*out_0_x0),[x4] "=&r" (trashed_x4)
        |                                                                                   ^
  A005.c:299:14: note: use constraint modifier "w"
    299 | "msr elr_el1,%[x4]\n"
        |              ^~~~~
        |              %w[x4]
  2 warnings generated.
  $ "./$TEST.exe" | sed '$d'
  Test A005 Required
  Histogram (1 states)
  4000000*>0:X0=1; 0:X2=0; 0:X3=1;
  No
  
  Witnesses
  Positive: 0, Negative: 4000000
  Condition forall (0:X0=0 /\ 0:X2=1 /\ 0:X3=1) is NOT validated
  Hash=63fe95c5d285e45a9e941fb36d381f70
  Observation A005 Never 0 4000000
