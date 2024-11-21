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
if [ ! -f example3.no ]; then
cat <<'EOF'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Results for example3.litmus %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
AArch64 example

{
 0:X0=x; 0:X1=42;
}
 P0          ;
 STR X1,[X0] ;
 XPACD X0    ;
 LDR X2,[X0] ;

exists (0:X2=42)
Generated assembler
EOF
cat ${TDIR}/example3.t
dorun ./example3.flat -q $LITMUSOPTS
fi
sleep $SLEEP

cat <<'EOF'
Revision c9853d399e12c3ae1df4799e69159a045c7c0996, version 7.57+1
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
