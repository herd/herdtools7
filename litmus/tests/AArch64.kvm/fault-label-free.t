A label-free fault predicate must compile and match an unlabeled instruction.

  $ mkdir LabelFree
  $ cat > LabelFree.litmus <<'LITMUS'
  > AArch64 LabelFree
  > Variant=memtag
  > {0:X0=x:red;}
  > P0;
  > LDR W1,[X0];
  > forall fault(P0,x)
  > LITMUS
  $ litmus7 -set-libdir ../../libdir -o LabelFree LabelFree.litmus -mach kvm-armv8.5-a+memtag -a 2 -s 10 -r 1
  $ cd LabelFree
  $ make SRCDIR="$KVM_UNIT_TESTS" > /dev/null
  $ TEST_DIR=$PWD
  $ (cd "$KVM_UNIT_TESTS" && TIMEOUT=30s ACCEL=tcg KVM_RUN=./arm/run sh -e "$TEST_DIR/run.sh") | sed -n '/^EXIT:/q; /^Test /,$p' > run.log
  $ grep -E '^20|^Positive:' run.log
  20    :> fault(P0,x,TagCheck);
  Positive: 20, Negative: 0
