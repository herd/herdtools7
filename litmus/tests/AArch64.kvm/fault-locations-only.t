Faults listed only in locations must compile and run without fault predicates
in the postcondition.

  $ TEST="LocationsOnly"
  $ cat > "$TEST.litmus" <<'EOF'
  > AArch64 LocationsOnly
  > Variant=memtag
  > {
  >  0:X0=x:red;
  > }
  > P0            ;
  > MOV W2,#1     ;
  > L0: LDR W1,[X0];
  > locations [fault(P0:L0,x);]
  > forall 0:X2=1
  > EOF
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -o "$TEST" "$TEST.litmus" \
  > -mach kvm-armv8.5-a+memtag \
  > -a 2 -s 10 -r 1

  $ cd "$TEST"
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=30s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -n '/^EXIT:/q; /^Test /,$p' > run.log
  $ grep -E '^Histogram|^20|^Positive:|^Faults ' run.log
  Histogram (1 states)
  20    :>0:X2=1;
  Positive: 20, Negative: 0
  Faults LocationsOnly 20 P0:20

  $ grep -F ' fault(P0:L0' run.log
  [1]

  $ grep -F '~fault(P0:L0' run.log
  [1]
