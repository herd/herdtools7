#!/bin/sh

date
LITMUSOPTS="${@:-$LITMUSOPTS}"
TDIR=$(dirname $0)
KVM_RUN="${KVM_RUN:-./arm-run}"
dorun () {
  EXE=$1
  shift
  OPTS="$@"
  ${KVM_RUN} ${TDIR}/${EXE} -smp 4 -append "${OPTS}"
}
SLEEP=0
cat <<'EOF'
Revision 702c1bc4911a46d2a8b60eb439197ccc8c998c0d, version 7.57+1
Command line: ../_build/install/default/bin/litmus7 -set-libdir ../litmus/libdir example3.litmus -mach kvm-m1 -variant fatal -o build/example3.tar
Parameters
#define SIZE_OF_TEST 5000
#define NUMBER_OF_RUN 200
#define AVAIL 4
/* gcc options: -Wall -std=gnu99 -std=gnu99 -ffreestanding -I $(SRCDIR)/lib -I $(SRCDIR)/libfdt -Wall -Werror  -fomit-frame-pointer -Wno-frame-address   -fno-pic  -no-pie -Wmissing-parameter-type -Wold-style-declaration -Woverride-init -O2 $(call optional-ccopt, -mno-outline-atomics) -march=armv8.1-a */
/* barrier: userfence */
/* alloc: static */
/* proc used: 4 */
EOF
echo "LITMUSOPTS=$LITMUSOPTS"
date
