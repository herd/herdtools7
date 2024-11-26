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
if [ ! -f example1.no ]; then
cat <<'EOF'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Results for example1.litmus %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
AArch64 example

{
 0:X1=x; 0:X3=y;
 1:X1=y; 1:X3=x;
}
 P0          | P1           ;
 MOV W0,#1   | LDR W0,[X1]  ;
             | PACDA X3, X5 ;
 STR W0,[X1] | LDR W2,[X3]  ;
 MOV W2,#1   |              ;
 DMB SY      |              ;
 STR W2,[X3] |              ;

exists (1:X0=1 /\ 1:X2=0)
Generated assembler
EOF
cat ${TDIR}/example1.t
dorun ./example1.flat -q $LITMUSOPTS
fi
sleep $SLEEP

cat <<'EOF'
Revision 702c1bc4911a46d2a8b60eb439197ccc8c998c0d, version 7.57+1
Command line: ../_build/install/default/bin/litmus7 -set-libdir ../litmus/libdir example1.litmus -mach kvm-m1 -variant fatal -o build/example1.tar
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
