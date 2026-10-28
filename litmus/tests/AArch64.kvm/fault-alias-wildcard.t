A fault predicate without labels must match both aliased and unlabeled faults.
A fault predicate on a thread must not make unobserved aliases on other threads
appear in the histogram.

  $ TEST="AliasWildcard"
  $ cat > "$TEST.litmus" <<'LITMUS'
  > AArch64 AliasWildcard
  > Variant=memtag,faultToNext
  > {0:X0=x:red; 1:X0=x:red; y=0;}
  > P0            | P1            ;
  > First:        | Other0:       ;
  > Second:       | Other1:       ;
  >               | Other2:         ;
  > LDR W1,[X0]   | LDR W1,[X0]   ;
  > LDR W2,[X0]   |               ;
  > forall fault(P0,x,TagCheck) /\ fault(P1:Other2,x,TagCheck) /\ ~fault(P1:Other2,y) /\ ~fault(P1:Other2,UndefinedInstruction)
  > LITMUS
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "$TEST.litmus" \
  > -mach kvm-armv8.5-a+memtag \
  > -a 2 -s 10 -r 1

  $ cd "$TEST"
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=30s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -n '/^EXIT:/q; /^Test /,$p' | grep -v '^Time'
  Test AliasWildcard Required
  Histogram (1 states)
  10    :> fault(P0,x,TagCheck); fault(P1:Other2,x,TagCheck); ~fault(P1:Other2,UndefinedInstruction); ~fault(P1:Other2,y);
  Ok
  Witnesses
  Positive: 10, Negative: 0
  Condition forall (fault(P0,x,TagCheck) /\ fault(P1:Other2,x,TagCheck) /\ not (fault(P1:Other2,y)) /\ not (fault(P1:Other2,UndefinedInstruction))) is validated
  Hash=6f80cdbc6641a07f5d9fbc6ae1da1742
  Variant=memtag,faultToNext
  Observation AliasWildcard Always 10 0
  Faults AliasWildcard 30 P0:20 P1:10
  
  


