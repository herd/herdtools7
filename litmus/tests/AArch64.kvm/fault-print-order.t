Faults from different unlabeled instructions must form one histogram state
when their data symbol and fault type match.

  $ TEST="FaultPrintOrder"
  $ cat > "$TEST.litmus" <<'LITMUS'
  > AArch64 FaultPrintOrder
  > Variant=memtag,faultToNext
  > {0:X0=x:red; 0:X1=x; 0:X4=y; 1:X0=y;}
  > P0                       | P1          ;
  > LDR W8,[X4]              | MOV W1,#1   ;
  > CMP W8,#0                | STR W1,[X0] ;
  > CSEL X2,X0,X1,EQ         |             ;
  > CSEL X3,X1,X0,EQ         |             ;
  > LDR W5,[X2]              |             ;
  > Middle: LDR W6,[X0]      |             ;
  > LDR W7,[X3]              |             ;
  > forall fault(P0,x,TagCheck) /\ fault(P0:Middle,x,TagCheck)
  > LITMUS
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" \
  > "$TEST.litmus" \
  > -mach kvm-armv8.5-a+memtag \
  > -a 2 -s 1000 -r 5

  $ cd "$TEST"
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=60s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -n '/^EXIT:/q; /^Test /,$p' | grep -v '^Time'
  Test FaultPrintOrder Required
  Histogram (1 states)
  5000  :> fault(P0,x,TagCheck); fault(P0:Middle,x,TagCheck);
  Ok
  Witnesses
  Positive: 5000, Negative: 0
  Condition forall (fault(P0,x,TagCheck) /\ fault(P0:Middle,x,TagCheck)) is validated
  Hash=ffdf1e3d3764bcc126287ce685117019
  Variant=memtag,faultToNext
  Observation FaultPrintOrder Always 5000 0
  Faults FaultPrintOrder 10000 P0:10000
  
  
