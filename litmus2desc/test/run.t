Run litmus2desc on all tests in sorted order and compare output.

  $ for f in $(LC_ALL=C ls -1 litmus_tests | sort); do
  >   echo "== $f =="; echo;
  >   cat "litmus_tests/$f"; echo;
  >   litmus2desc "litmus_tests/$f"; echo;
  > done
  == CoRR.litmus ==
  
  AArch64 CoRR
  {
  0:X1=x;
  1:X0=x;
  }
   P0          | P1          ;
   MOV W0,#1   | LDR W1,[X0] ;
   STR W0,[X1] | LDR W2,[X0] ;
  exists (1:X1=1 /\ 1:X2=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The second Load instruction on P1, i.e. LDR W2,[X0], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P0;
  The Register X2 on P1 holds the value 0 in the end, because the second Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the Explicit Memory Write Effect on P0;
  
  == CoRW1.litmus ==
  
  AArch64 CoRW1
  { 0:X0=x; }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#1   ;
   STR W2,[X0] ;
  exists (0:X1=1)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Load instruction on P0, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The Store instruction on P0, i.e. STR W2,[X0], generates an Explicit Memory Write Effect of Location x with value 1;
  The Register X1 on P0 holds the value 1 in the end, because the Explicit Memory Read Effect has read the value 1 from the Explicit Memory Write Effect;
  
  == CoRW2.litmus ==
  
  AArch64 A
  "PosRW Coe Rfe"
  Generator=diyone7 (version 7.58)
  Com=Co Rf
  Orig=PosRW Coe Rfe
  {
  0:X0=x;
  1:X1=x;
  }
   P0          | P1          ;
   LDR W1,[X0] | MOV W0,#2   ;
   MOV W2,#1   | STR W0,[X1] ;
   STR W2,[X0] |             ;
  exists ([x]=2 /\ 0:X1=2)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Load instruction on P0, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 2;
  The Store instruction on P0, i.e. STR W2,[X0], generates an Explicit Memory Write Effect of Location x with value 1;
  The Store instruction on P1, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 2;
  The Register X1 on P0 holds the value 2 in the end, because the Explicit Memory Read Effect on P0 has read the value 2 from the Explicit Memory Write Effect on P1;
  The value of x is 2 in the end, because the Explicit Memory Write Effect on P0 is Coherence-before the Explicit Memory Write Effect on P1;
  
  == CoWR.litmus ==
  
  AArch64 CoWR
  { 0:X1=x; }
   P0          ;
   MOV W0,#1   ;
   STR W0,[X1] ;
   LDR W2,[X1] ;
  exists (0:X2=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The Load instruction on P0, i.e. LDR W2,[X1], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X2 on P0 holds the value 0 in the end, because the Explicit Memory Read Effect has read the value 0 from the initialization event of location x. In other words, the Explicit Memory Read Effect is Coherence-before the Explicit Memory Write Effect;
  
  == CoWW.litmus ==
  
  AArch64 CoWW
  { 0:X1=x; }
   P0          ;
   MOV W0,#1   ;
   STR W0,[X1] ;
   MOV W2,#2   ;
   STR W2,[X1] ;
  exists ([x]=1)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X1], generates an Explicit Memory Write Effect of Location x with value 2;
  The value of x is 1 in the end, because the second Explicit Memory Write Effect is Coherence-before the first Explicit Memory Write Effect;
  
  == ISA2.litmus ==
  
  AArch64 ISA2
  "PodWW Rfe PodRW Rfe PodRR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:z=W,2:z=F,2:x=T
  Com=Rf Rf Fr
  Orig=PodWW Rfe PodRW Rfe PodRR Fre
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X3=z;
  2:X0=z; 2:X2=x;
  }
   P0          | P1          | P2          ;
   MOV W0,#1   | LDR W1,[X0] | LDR W1,[X0] ;
   STR W0,[X1] | MOV W2,#1   | LDR W3,[X2] ;
   MOV W2,#1   | STR W2,[X3] |             ;
   STR W2,[X3] |             |             ;
  exists (1:X1=1 /\ 2:X1=1 /\ 2:X3=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location z with value 1;
  The first Load instruction on P2, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location z with value 1;
  The second Load instruction on P2, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X1 on P2 holds the value 1 in the end, because the first Explicit Memory Read Effect on P2 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X3 on P2 holds the value 0 in the end, because the second Explicit Memory Read Effect on P2 has read the value 0 from the initialization event of location x. In other words, the second Explicit Memory Read Effect on P2 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == LB+addr+ctrl.litmus ==
  
  AArch64 LB+addr+ctrl
  "DpAddrdW Rfe DpCtrldW Rfe"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf
  Orig=DpAddrdW Rfe DpCtrldW Rfe
  {
  0:X0=x; 0:X4=y;
  1:X0=y; 1:X3=x;
  }
   P0                  | P1           ;
   LDR W1,[X0]         | LDR W1,[X0]  ;
   EOR W2,W1,W1        | CBNZ W1,LC00 ;
   MOV W3,#1           | LC00:        ;
   STR W3,[X4,W2,SXTW] | MOV W2,#1    ;
                       | STR W2,[X3]  ;
  exists (0:X1=1 /\ 1:X1=1)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Load instruction on P0, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The Exclusive Or instruction on P0, i.e. EOR W2,W1,W1, creates an Address dependency from the Load instruction on P0, i.e. LDR W1,[X0], to the Store instruction on P0, i.e. STR W3,[X4,W2,SXTW];
  The Store instruction on P0, i.e. STR W3,[X4,W2,SXTW], generates an Explicit Memory Write Effect of Location y with value 1;
  The Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Branch instruction on P1 creates a Control dependency from the Load instruction on P1, i.e. LDR W1,[X0], to the Store instruction on P1, i.e. STR W2,[X3];
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location x with value 1;
  The Register X1 on P0 holds the value 1 in the end, because the Explicit Memory Read Effect on P0 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X1 on P1 holds the value 1 in the end, because the Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P0;
  
  == LB.litmus ==
  
  AArch64 LB
  "PodRW Rfe PodRW Rfe"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf
  Orig=PodRW Rfe PodRW Rfe
  {
  0:X0=x; 0:X3=y;
  1:X0=y; 1:X3=x;
  }
   P0          | P1          ;
   LDR W1,[X0] | LDR W1,[X0] ;
   MOV W2,#1   | MOV W2,#1   ;
   STR W2,[X3] | STR W2,[X3] ;
  exists (0:X1=1 /\ 1:X1=1)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Load instruction on P0, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location x with value 1;
  The Register X1 on P0 holds the value 1 in the end, because the Explicit Memory Read Effect on P0 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X1 on P1 holds the value 1 in the end, because the Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P0;
  
  == MP+poll+poaa-amo.casal-pola.litmus ==
  
  AArch64 MP+poll+poaa-amo.casal-pola
  "PodWWLL RfeLA PodRRAA Amo.CasAL PodWRLA FreAL"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Rf Fr
  Orig=PodWWLL RfeLA PodRRAA Amo.CasAL PodWRLA FreAL
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X2=z; 1:X5=x;
  }
   P0           | P1               ;
   MOV W0,#1    | LDAR W1,[X0]     ;
   STLR W0,[X1] | MOV W3,#0        ;
   MOV W2,#1    | MOV W4,#1        ;
   STLR W2,[X3] | CASAL W3,W4,[X2] ;
                | LDAR W6,[X5]     ;
  exists (1:X1=1 /\ 1:X3=0 /\ 1:X6=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STLR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STLR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The first Load instruction on P1, i.e. LDAR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The second Load instruction on P1, i.e. CASAL W3,W4,[X2], generates an Explicit Memory Read Effect of Location z with value 0;
  The Store instruction on P1, i.e. CASAL W3,W4,[X2], generates an Explicit Memory Write Effect of Location z with value 1;
  The third Load instruction on P1, i.e. LDAR W6,[X5], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the second Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location z. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the Explicit Memory Write Effect on P1;
  The Register X6 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == MP.litmus ==
  
  AArch64 MP
  "PodWW Rfe PodRR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Rf Fr
  Orig=PodWW Rfe PodRR Fre
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X2=x;
  }
   P0          | P1          ;
   MOV W0,#1   | LDR W1,[X0] ;
   STR W0,[X1] | LDR W3,[X2] ;
   MOV W2,#1   |             ;
   STR W2,[X3] |             ;
  exists (1:X1=1 /\ 1:X3=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The second Load instruction on P1, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the second Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == PPOAC.litmus ==
  
  AArch64 A
  "PodWW Rfe DpAddrdW Rfi DpCtrldR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Rf Fr
  Orig=PodWW Rfe DpAddrdW Rfi DpCtrldR Fre
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X4=z; 1:X6=x;
  }
   P0          | P1                  ;
   MOV W0,#1   | LDR W1,[X0]         ;
   STR W0,[X1] | EOR W2,W1,W1        ;
   MOV W2,#1   | MOV W3,#1           ;
   STR W2,[X3] | STR W3,[X4,W2,SXTW] ;
               | LDR W5,[X4]         ;
               | CBNZ W5,LC00        ;
               | LC00:               ;
               | LDR W7,[X6]         ;
  exists (1:X1=1 /\ 1:X5=1 /\ 1:X7=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Exclusive Or instruction on P1, i.e. EOR W2,W1,W1, creates an Address dependency from the first Load instruction on P1, i.e. LDR W1,[X0], to the Store instruction on P1, i.e. STR W3,[X4,W2,SXTW];
  The Store instruction on P1, i.e. STR W3,[X4,W2,SXTW], generates an Explicit Memory Write Effect of Location z with value 1;
  The second Load instruction on P1, i.e. LDR W5,[X4], generates an Explicit Memory Read Effect of Location z with value 1;
  The Branch instruction on P1 does not create any dependency from the first Load instruction on P1, i.e. LDR W1,[X0], to the third Load instruction on P1, i.e. LDR W7,[X6];
  The Branch instruction on P1 does not create any dependency from the second Load instruction on P1, i.e. LDR W5,[X4], to the third Load instruction on P1, i.e. LDR W7,[X6];
  The third Load instruction on P1, i.e. LDR W7,[X6], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X5 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X7 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == PPOCC.litmus ==
  
  AArch64 PPOCC
  "PodWW Rfe DpCtrldW Rfi DpCtrldR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Rf Fr
  Orig=PodWW Rfe DpCtrldW Rfi DpCtrldR Fre
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X3=z; 1:X5=x;
  }
   P0          | P1           ;
   MOV W0,#1   | LDR W1,[X0]  ;
   STR W0,[X1] | CBNZ W1,LC00 ;
   MOV W2,#1   | LC00:        ;
   STR W2,[X3] | MOV W2,#1    ;
               | STR W2,[X3]  ;
               | LDR W4,[X3]  ;
               | CBNZ W4,LC01 ;
               | LC01:        ;
               | LDR W6,[X5]  ;
  exists (1:X1=1 /\ 1:X4=1 /\ 1:X6=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The first Branch instruction on P1 creates a Control dependency from the first Load instruction on P1, i.e. LDR W1,[X0], to the Store instruction on P1, i.e. STR W2,[X3];
  The first Branch instruction on P1 does not create any dependency from the first Load instruction on P1, i.e. LDR W1,[X0], to the second Load instruction on P1, i.e. LDR W4,[X3];
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location z with value 1;
  The second Load instruction on P1, i.e. LDR W4,[X3], generates an Explicit Memory Read Effect of Location z with value 1;
  The second Branch instruction on P1 does not create any dependency from the second Load instruction on P1, i.e. LDR W4,[X3], to the third Load instruction on P1, i.e. LDR W6,[X5];
  The third Load instruction on P1, i.e. LDR W6,[X5], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X4 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X6 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == PPODC.litmus ==
  
  AArch64 PPODC
  "PodWW Rfe DpDatadW Rfi DpCtrldR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Rf Fr
  Orig=PodWW Rfe DpDatadW Rfi DpCtrldR Fre
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X3=z; 1:X5=x;
  }
   P0          | P1           ;
   MOV W0,#1   | LDR W1,[X0]  ;
   STR W0,[X1] | EOR W2,W1,W1 ;
   MOV W2,#1   | ADD W2,W2,#1 ;
   STR W2,[X3] | STR W2,[X3]  ;
               | LDR W4,[X3]  ;
               | CBNZ W4,LC00 ;
               | LC00:        ;
               | LDR W6,[X5]  ;
  exists (1:X1=1 /\ 1:X4=1 /\ 1:X6=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Exclusive Or instruction on P1, i.e. EOR W2,W1,W1, and the Add instruction on P1, i.e. ADD W2,W2,#1, create a Data dependency from the first Load instruction on P1, i.e. LDR W1,[X0], to the Store instruction on P1, i.e. STR W2,[X3];
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location z with value 1;
  The second Load instruction on P1, i.e. LDR W4,[X3], generates an Explicit Memory Read Effect of Location z with value 1;
  The Branch instruction on P1 does not create any dependency from the first Load instruction on P1, i.e. LDR W1,[X0], to the third Load instruction on P1, i.e. LDR W6,[X5];
  The Branch instruction on P1 does not create any dependency from the second Load instruction on P1, i.e. LDR W4,[X3], to the third Load instruction on P1, i.e. LDR W6,[X5];
  The third Load instruction on P1, i.e. LDR W6,[X5], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X4 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X6 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == W+RR+WR+WR.litmus ==
  
  AArch64 W+RR+WR+WR
  "Rfe PodRR Fre PodWR Fre PodWR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=1:x=F,1:y=T,2:y=F,2:z=T,3:z=F,3:x=T
  Com=Rf Fr Fr Fr
  Orig=Rfe PodRR Fre PodWR Fre PodWR Fre
  {
  0:X1=x;
  1:X0=x; 1:X2=y;
  2:X1=y; 2:X2=z;
  3:X1=z; 3:X2=x;
  }
   P0          | P1          | P2          | P3          ;
   MOV W0,#1   | LDR W1,[X0] | MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | LDR W3,[X2] | STR W0,[X1] | STR W0,[X1] ;
               |             | LDR W3,[X2] | LDR W3,[X2] ;
  exists (1:X1=1 /\ 1:X3=0 /\ 2:X3=0 /\ 3:X3=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The second Load instruction on P1, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location y with value 0;
  The Store instruction on P2, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location y with value 1;
  The Load instruction on P2, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location z with value 0;
  The Store instruction on P3, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location z with value 1;
  The Load instruction on P3, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the first Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the second Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location y. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the Explicit Memory Write Effect on P2;
  The Register X3 on P2 holds the value 0 in the end, because the Explicit Memory Read Effect on P2 has read the value 0 from the initialization event of location z. In other words, the Explicit Memory Read Effect on P2 is Coherence-before the Explicit Memory Write Effect on P3;
  The Register X3 on P3 holds the value 0 in the end, because the Explicit Memory Read Effect on P3 has read the value 0 from the initialization event of location x. In other words, the Explicit Memory Read Effect on P3 is Coherence-before the Explicit Memory Write Effect on P0;
  
  == W+RW+R+WR.litmus ==
  
  AArch64 W+RW+R+WR
  "Rfe PodRW Rfe Fre PodWR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=1:x=F,1:y=W,3:y=F,3:x=T
  Com=Rf Rf Fr Fr
  Orig=Rfe PodRW Rfe Fre PodWR Fre
  {
  0:X1=x;
  1:X0=x; 1:X3=y;
  2:X0=y;
  3:X1=y; 3:X2=x;
  }
   P0          | P1          | P2          | P3          ;
   MOV W0,#1   | LDR W1,[X0] | LDR W1,[X0] | MOV W0,#2   ;
   STR W0,[X1] | MOV W2,#1   |             | STR W0,[X1] ;
               | STR W2,[X3] |             | LDR W3,[X2] ;
  exists ([y]=2 /\ 1:X1=1 /\ 2:X1=1 /\ 3:X3=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The Load instruction on P2, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Store instruction on P3, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location y with value 2;
  The Load instruction on P3, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P0;
  The Register X1 on P2 holds the value 1 in the end, because the Explicit Memory Read Effect on P2 has read the value 1 from the Explicit Memory Write Effect on P1. In other words, the Explicit Memory Read Effect on P2 is Coherence-before the Explicit Memory Write Effect on P3;
  The Register X3 on P3 holds the value 0 in the end, because the Explicit Memory Read Effect on P3 has read the value 0 from the initialization event of location x. In other words, the Explicit Memory Read Effect on P3 is Coherence-before the Explicit Memory Write Effect on P0;
  The value of y is 2 in the end, because the Explicit Memory Write Effect on P1 is Coherence-before the Explicit Memory Write Effect on P3;
  
  == WRC.litmus ==
  
  AArch64 WRC
  "Rfe PodRW Rfe PodRR Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=1:x=F,1:y=W,2:y=F,2:x=T
  Com=Rf Rf Fr
  Orig=Rfe PodRW Rfe PodRR Fre
  {
  0:X1=x;
  1:X0=x; 1:X3=y;
  2:X0=y; 2:X2=x;
  }
   P0          | P1          | P2          ;
   MOV W0,#1   | LDR W1,[X0] | LDR W1,[X0] ;
   STR W0,[X1] | MOV W2,#1   | LDR W3,[X2] ;
               | STR W2,[X3] |             ;
  exists (1:X1=1 /\ 2:X1=1 /\ 2:X3=0)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The first Load instruction on P2, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The second Load instruction on P2, i.e. LDR W3,[X2], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 1 in the end, because the Explicit Memory Read Effect on P1 has read the value 1 from the Explicit Memory Write Effect on P0;
  The Register X1 on P2 holds the value 1 in the end, because the first Explicit Memory Read Effect on P2 has read the value 1 from the Explicit Memory Write Effect on P1;
  The Register X3 on P2 holds the value 0 in the end, because the second Explicit Memory Read Effect on P2 has read the value 0 from the initialization event of location x. In other words, the second Explicit Memory Read Effect on P2 is Coherence-before the Explicit Memory Write Effect on P0;
  
  == WW+RW+R.litmus ==
  
  AArch64 WW+RW+R
  "PodWW Rfe PodRW Rfe Fre"
  Generator=diyone7 (version 7.58)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf Fr
  Orig=PodWW Rfe PodRW Rfe Fre
  {
  0:X1=x; 0:X3=y;
  1:X0=y; 1:X3=x;
  2:X0=x;
  }
   P0          | P1          | P2          ;
   MOV W0,#2   | LDR W1,[X0] | LDR W1,[X0] ;
   STR W0,[X1] | MOV W2,#1   |             ;
   MOV W2,#1   | STR W2,[X3] |             ;
   STR W2,[X3] |             |             ;
  exists ([x]=2 /\ 1:X1=1 /\ 2:X1=1)
  
  This litmus test asks whether the following execution is architecturally allowed:
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 2;
  The second Store instruction on P0, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location y with value 1;
  The Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location y with value 1;
  The Store instruction on P1, i.e. STR W2,[X3], generates an Explicit Memory Write Effect of Location x with value 1;
  The Load instruction on P2, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The Register X1 on P1 holds the value 1 in the end, because the Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0;
  The Register X1 on P2 holds the value 1 in the end, because the Explicit Memory Read Effect on P2 has read the value 1 from the Explicit Memory Write Effect on P1. In other words, the Explicit Memory Read Effect on P2 is Coherence-before the first Explicit Memory Write Effect on P0;
  The value of x is 2 in the end, because the Explicit Memory Write Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  
  == multi_execs.litmus ==
  
  AArch64 multi_execs
  "PosWW PosWW Rfe PosRR Fre"
  Generator=diyone7 (version 7.58)
  Com=Rf Fr
  Orig=PosWW PosWW Rfe PosRR Fre
  {
  0:X1=x;
  1:X0=x;
  }
   P0          | P1          ;
   MOV W0,#1   | LDR W1,[X0] ;
   STR W0,[X1] | LDR W2,[X0] ;
   MOV W2,#1   | LDR W3,[X0] ;
   STR W2,[X1] |             ;
   MOV W3,#3   |             ;
   STR W3,[X1] |             ;
  exists ([x]=3 /\ 1:X1=3 /\ 1:X2=1 /\ 1:X3=0)
  
  This litmus tests asks whether one of the following candidate executions is architecturally allowed:
  
  === first execution ===
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The third Store instruction on P0, i.e. STR W3,[X1], generates an Explicit Memory Write Effect of Location x with value 3;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 3;
  The second Load instruction on P1, i.e. LDR W2,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The third Load instruction on P1, i.e. LDR W3,[X0], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 3 in the end, because the first Explicit Memory Read Effect on P1 has read the value 3 from the third Explicit Memory Write Effect on P0;
  The Register X2 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the third Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  The value of x is 3 in the end, because the second Explicit Memory Write Effect on P0 is Coherence-before the third Explicit Memory Write Effect on P0;
  
  === second execution ===
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The third Store instruction on P0, i.e. STR W3,[X1], generates an Explicit Memory Write Effect of Location x with value 3;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 3;
  The second Load instruction on P1, i.e. LDR W2,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The third Load instruction on P1, i.e. LDR W3,[X0], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 3 in the end, because the first Explicit Memory Read Effect on P1 has read the value 3 from the third Explicit Memory Write Effect on P0;
  The Register X2 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the second Explicit Memory Write Effect on P0. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the second Explicit Memory Write Effect on P0;
  The value of x is 3 in the end, because the first Explicit Memory Write Effect on P0 is Coherence-before the third Explicit Memory Write Effect on P0;
  
  === third execution ===
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The third Store instruction on P0, i.e. STR W3,[X1], generates an Explicit Memory Write Effect of Location x with value 3;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 3;
  The second Load instruction on P1, i.e. LDR W2,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The third Load instruction on P1, i.e. LDR W3,[X0], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 3 in the end, because the first Explicit Memory Read Effect on P1 has read the value 3 from the third Explicit Memory Write Effect on P0;
  The Register X2 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the first Explicit Memory Write Effect on P0. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the second Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the first Explicit Memory Write Effect on P0;
  The value of x is 3 in the end, because the second Explicit Memory Write Effect on P0 is Coherence-before the third Explicit Memory Write Effect on P0;
  
  === fourth execution ===
  
  The first Store instruction on P0, i.e. STR W0,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The second Store instruction on P0, i.e. STR W2,[X1], generates an Explicit Memory Write Effect of Location x with value 1;
  The third Store instruction on P0, i.e. STR W3,[X1], generates an Explicit Memory Write Effect of Location x with value 3;
  The first Load instruction on P1, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x with value 3;
  The second Load instruction on P1, i.e. LDR W2,[X0], generates an Explicit Memory Read Effect of Location x with value 1;
  The third Load instruction on P1, i.e. LDR W3,[X0], generates an Explicit Memory Read Effect of Location x with value 0;
  The Register X1 on P1 holds the value 3 in the end, because the first Explicit Memory Read Effect on P1 has read the value 3 from the third Explicit Memory Write Effect on P0;
  The Register X2 on P1 holds the value 1 in the end, because the second Explicit Memory Read Effect on P1 has read the value 1 from the first Explicit Memory Write Effect on P0. In other words, the second Explicit Memory Read Effect on P1 is Coherence-before the third Explicit Memory Write Effect on P0;
  The Register X3 on P1 holds the value 0 in the end, because the third Explicit Memory Read Effect on P1 has read the value 0 from the initialization event of location x. In other words, the third Explicit Memory Read Effect on P1 is Coherence-before the second Explicit Memory Write Effect on P0;
  The value of x is 3 in the end, because the first Explicit Memory Write Effect on P0 is Coherence-before the third Explicit Memory Write Effect on P0;
  
