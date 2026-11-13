Multiple labels on one instruction must be accepted, including when the fault
condition refers to either label.
Compile and run the generated tests with KVM unit tests and QEMU TCG.
Set KVM_UNIT_TESTS to an absolute path to a configured, built checkout.

  $ TEST="T"
  $ cat > "$TEST.litmus" <<'EOF'
  > AArch64 T
  > Variant=memtag
  > {
  >  0:X0=x:red;
  > }
  >  P0         ;
  > L0:         ;
  > L1:         ;
  >  LDR W1,[X0];
  > forall fault(P0:L0,x) /\ fault(P0:L1,x)
  > EOF
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" "$TEST.litmus" \
  > -mach kvm-armv8.5-a+memtag \
  > -a 2 -s 10 -r 1
  $ cd "$TEST"
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=30s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -n '/^EXIT:/q; /^Test /,$p' | grep -v '^Time'
  Test T Required
  Histogram (1 states)
  20    :> fault(P0:{L0,L1},x,TagCheck);
  Ok
  Witnesses
  Positive: 20, Negative: 0
  Condition forall (fault(P0:L0,x) /\ fault(P0:L1,x)) is validated
  Hash=88e143778d794db619a999f72ae5074c
  Variant=memtag
  Observation T Always 20 0
  Faults T 20 P0:20
  
  

