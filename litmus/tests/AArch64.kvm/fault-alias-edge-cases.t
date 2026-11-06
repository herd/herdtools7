Exercise alias collection and matching beyond the two-label case. The first
fault has three aliases and the second has two. Only the last alias of each
group is in the postcondition. The safe instruction must produce a negative
fault observation.

  $ TEST="AliasEdges"
  $ cat > "$TEST.litmus" <<'LITMUS'
  > AArch64 AliasEdges
  > Variant=memtag,faultToNext
  > {0:X0=x:red;}
  > P0            ;
  > A0:           ;
  > A1:           ;
  > A2:           ;
  > LDR W1,[X0]   ;
  > Later0:       ;
  > Later1:       ;
  > LDR W2,[X0]   ;
  > Safe: NOP     ;
  > locations [fault(P0:A0,x); fault(P0:Later0,x);]
  > forall fault(P0:A2,x,TagCheck) /\ fault(P0:Later1,x,TagCheck) /\ ~fault(P0:Safe)
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
  Test AliasEdges Required
  Histogram (1 states)
  20    :> fault(P0:A2,x,TagCheck); fault(P0:Later1,x,TagCheck); ~fault(P0:Safe);
  Ok
  Witnesses
  Positive: 20, Negative: 0
  Condition forall (fault(P0:A2,x,TagCheck) /\ fault(P0:Later1,x,TagCheck) /\ not (fault(P0:Safe))) is validated
  Hash=0245ad8b0364c45a963896076b20d8b9
  Variant=memtag,faultToNext
  Observation AliasEdges Always 20 0
  Faults AliasEdges 40 P0:40
  
  
