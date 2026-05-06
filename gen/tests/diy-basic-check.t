A test for no metadata, `-metadata false`
  $ diyone7 -arch AArch64 -variant vmsa PteOA PosWW PteOA PteV1 PteAF0 PosWR PteHA Fri -oneloc -metadata false
  AArch64 CoWR0+posWpteoapteoa.v1.af0-pospteoa.v1.af0pteha-friptehapteoa
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0, valid:0);
   [y]=5;
   0:X0=PTE(x); 0:X1=(oa:PA(y), af:0, valid:0); 0:X2=(oa:PA(y)); 0:X3=x;
  }
   P0               ;
   STR X1,[X0]      ;
   STR X2,[X0]      ;
   L00: LDR W4,[X3] ;
  
  exists (fault(P0:L00,x))
A diy7 predicate merge test for before/after boundary predicates
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[@before(Po) PodRW Rfe @after(Po)]' -safe Po 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [@before(Po) PodRW Rfe @after(Po)] -safe Po
  Generator produced 2 tests
  LB000: PosRR PodRW Rfe PosRR PodRW Rfe
  LB001: PodRR PodRW Rfe PodRR PodRW Rfe
A diy7 predicate merge test for repeated after predicates
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[PodRW Rfe @after(PodRW) @after(Rfe)]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [PodRW Rfe @after(PodRW) @after(Rfe)]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 predicate merge test for after on composite relaxations
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[PodRW Rfe @after([PodRW Rfe])]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [PodRW Rfe @after([PodRW Rfe])]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A diy7 predicate merge test for before on composite relaxations
  $ diy7 -arch AArch64 -cycleonly true -size 4 -relax '[@before([PodRW Rfe]) PodRW Rfe]' 2>&1 | grep -v '^# Version' | grep -v '^Relaxations tested:'
  # diy7 -arch AArch64 -cycleonly true -size 4 -relax [@before([PodRW Rfe]) PodRW Rfe]
  Generator produced 3 tests
  LB000: PodRW Rfe PodRW Rfe
  3.LB000: PodRW Rfe PodRW Rfe PodRW Rfe
  4.LB000: PodRW Rfe PodRW Rfe PodRW Rfe PodRW Rfe
A VMSA test for a negated exists check, `-neg true`
  $ diyone7 -arch AArch64 -variant vmsa Amo.Cas TLBI-sync.ISHdWW PteV1 PteAF0 PteOA Rfe Pte PodRW PteHD Rfe -neg true -info "User-define=User-define"
  AArch64 LB+popteptehd+amo.cas-tlbi-sync.ishppteoa.v1.af0
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf
  Orig=Amo.Cas TLBI-sync.ISHdWWPPteOA.V1.AF0 RfePteOA.V1.AF0Pte PodRWPtePteHD RfePteHDP
  User-define=User-define
  "Amo.Cas TLBI-sync.ISHdWWPPteOA.V1.AF0 RfePteOA.V1.AF0Pte PodRWPtePteHD RfePteHDP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   [y]=5;
   [PTE(y)]=(oa:PA(y), valid:0);
   0:X0=x; 0:X3=PTE(y); 0:X4=(oa:PA(x), af:0);
   1:X0=x; pteval_t 1:X1=0; 1:X3=PTE(y);
  }
   P0                  | P1               ;
   MOV W1,#2           | LDR X1,[X3]      ;
   MOV W2,#3           | MOV W2,#2        ;
   L01: CAS W1,W2,[X0] | L00: STR W2,[X0] ;
   DSB ISH             |                  ;
   LSR X5,X0,#12       |                  ;
   TLBI VAAE1IS,X5     |                  ;
   DSB ISH             |                  ;
   STR X4,[X3]         |                  ;
  
  ~exists ([x]=3 /\ 0:X1=2 /\ 1:X1=(oa:PA(x), af:0) /\ not (fault(P0:L01,x)) /\ not (fault(P1:L00,x)))
A VMSA test for observing locations, `-cond observe`
  $ diyone7 -arch AArch64 -variant vmsa Amo.Cas TLBI-sync.ISHdWW PteV1 PteAF0 PteOA Rfe Pte PodRW PteHD Rfe -info "User-define=User-define" -cond observe
  AArch64 LB+popteptehd+amo.cas-tlbi-sync.ishppteoa.v1.af0
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf
  Orig=Amo.Cas TLBI-sync.ISHdWWPPteOA.V1.AF0 RfePteOA.V1.AF0Pte PodRWPtePteHD RfePteHDP
  User-define=User-define
  "Amo.Cas TLBI-sync.ISHdWWPPteOA.V1.AF0 RfePteOA.V1.AF0Pte PodRWPtePteHD RfePteHDP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   [y]=5;
   [PTE(y)]=(oa:PA(y), valid:0);
   0:X0=x; 0:X3=PTE(y); 0:X4=(oa:PA(x), af:0);
   1:X0=x; pteval_t 1:X1=0; 1:X3=PTE(y);
  }
   P0                  | P1               ;
   MOV W1,#2           | LDR X1,[X3]      ;
   MOV W2,#3           | MOV W2,#2        ;
   L01: CAS W1,W2,[X0] | L00: STR W2,[X0] ;
   DSB ISH             |                  ;
   LSR X5,X0,#12       |                  ;
   TLBI VAAE1IS,X5     |                  ;
   DSB ISH             |                  ;
   STR X4,[X3]         |                  ;
  
  locations [x; 0:X1; 1:X1; fault(P0:L01,x); fault(P1:L00,x);]
  forall (true)
A VMSA test for a forall check, `-cond unicond`
  $ diyone7 -arch AArch64 -variant vmsa Amo.Cas TLBI-sync.ISHdWW PteV1 PteAF0 PteOA Rfe Pte PodRW PteHD Rfe -info "User-define=User-define" -cond unicond
  AArch64 LB+popteptehd+amo.cas-tlbi-sync.ishppteoa.v1.af0
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf
  Orig=Amo.Cas TLBI-sync.ISHdWWPPteOA.V1.AF0 RfePteOA.V1.AF0Pte PodRWPtePteHD RfePteHDP
  User-define=User-define
  "Amo.Cas TLBI-sync.ISHdWWPPteOA.V1.AF0 RfePteOA.V1.AF0Pte PodRWPtePteHD RfePteHDP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   [y]=5;
   [PTE(y)]=(oa:PA(y), valid:0);
   0:X0=x; 0:X3=PTE(y); 0:X4=(oa:PA(x), af:0);
   1:X0=x; pteval_t 1:X1=0; 1:X3=PTE(y);
  }
   P0                  | P1               ;
   MOV W1,#2           | LDR X1,[X3]      ;
   MOV W2,#3           | MOV W2,#2        ;
   L01: CAS W1,W2,[X0] | L00: STR W2,[X0] ;
   DSB ISH             |                  ;
   LSR X5,X0,#12       |                  ;
   TLBI VAAE1IS,X5     |                  ;
   DSB ISH             |                  ;
   STR X4,[X3]         |                  ;
  
  forall (not (fault(P0:L01,x)) /\ not (fault(P1:L00,x)) /\ ([y]=(oa:PA(x), af:0) /\ (0:X1=2 /\ ([x]=3 /\ (1:X1=(oa:PA(x), af:0) \/ 1:X1=0)) \/ 0:X1=0 /\ (1:X1=(oa:PA(x), af:0) /\ ([x]=3 \/ [x]=2) \/ 1:X1=0 /\ ([x]=3 \/ [x]=2)))))
A memtag generation test with `Variant` duplicated in metadata, because of (1) `-info "Variant=memtag"` and (2) automatically generated `Variant=memtag`
  $ diyone7 -arch AArch64 -variant memtag DpDatadW T PosWW T Rfe PodRW Rfe T -info "Variant=memtag"
  AArch64 LB+po+dataWtt-postt
  Variant=memtag memtag
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Rf
  Orig=DpDatadWTT PosWWTT RfeTP PodRW RfePT
  "DpDatadWTT PosWWTT RfeTP PodRW RfePT"
  {
   0:X1=x:green; 0:X3=y:red; 0:X5=y:green; 0:X6=y:blue;
   1:X1=x:green; 1:X6=y:blue;
  }
   P0                | P1               ;
   MOV X0,X1         | L00: LDR W0,[X6] ;
   LDG X0,[X1]       | MOV W2,#1        ;
   EOR X2,X0,X0      | STR W2,[X1]      ;
   ADD X4,X3,W2,SXTW |                  ;
   STG X4,[X5]       |                  ;
   STG X6,[X3]       |                  ;
  
  exists ([tag(y)]=:blue /\ 0:X0=x:green /\ 1:X0=0 /\ not (fault(P1:L00,y)))
An ifetch generation test
  $ diyone7 -arch AArch64 -variant ifetch CacheSyncStrongIsbdWRPI FreIP PodWR Fre
  AArch64 SB+cachesyncstrongisbpi+po
  Variant=ifetch
  Generator=diyone7 (version 7.58+1)
  Com=Fr Fr
  Orig=CacheSyncStrongIsbdWRPI FreIP PodWR Fre
  "CacheSyncStrongIsbdWRPI FreIP PodWR Fre"
  {
   0:X1=x; 0:X3=P0:Lself00;
   1:X0=instr:"NOP"; 1:X1=x; 1:X3=P0:Lself00;
  }
   P0              | P1          ;
   MOV W0,#1       | STR W0,[X3] ;
   STR W0,[X1]     | LDR W2,[X1] ;
   DC CIVAC,X3     |             ;
   DSB ISH         |             ;
   IC IVAU,X3      |             ;
   DSB ISH         |             ;
   ISB             |             ;
   Lself00: B .+12 |             ;
   MOV W2,#2       |             ;
   B .+8           |             ;
   MOV W2,#1       |             ;
  
  exists (0:X2=1 /\ 1:X2=0)
A base test with int64 arrays
  $ diyone7 -arch AArch64 -type int64_t X PodWW Coe PodWR Pa Fre
  AArch64 R+poxp+poppa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Co Fr
  Orig=PodWWXP Coe PodWRPPa FrePaX
  "PodWWXP Coe PodWRPPa FrePaX"
  {
   int64_t x[2]={0,0};
   int64_t y=0;
   0:X0=x; int64_t 0:X2=0; 0:X5=y;
   1:X0=x; int64_t 1:X2=0; 1:X5=y;
  }
   P0              | P1             ;
   MOV X1,#1       | MOV X1,#2      ;
   Loop00:         | STR X1,[X5]    ;
   LDXR X2,[X0]    | LDP X2,X3,[X0] ;
   STXR W3,X1,[X0] | ADD X2,X2,X3   ;
   CBNZ X3,Loop00  |                ;
   MOV X4,#1       |                ;
   STR X4,[X5]     |                ;
  
  exists (x={1,0} /\ [y]=2 /\ 0:X2=0 /\ 1:X2=0)
A C test for exists
  $ diyone7 -arch C PodWW Coe PodWR Fre
  Warning: optimised conditions are not supported by C arch
  C R
  "PodWW Coe PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=T
  Com=Co Fr
  Orig=PodWW Coe PodWR Fre
  
  {}
  
  P0 (volatile int* y,volatile int* x) {
    *x = 1;
    *y = 1;
  }
  
  P1 (volatile int* y,volatile int* x) {
    *y = 2;
    int r0 = *x;
  }
  
  exists ([y]=2 /\ 1:r0=0)
A C test for negated exists
  $ diyone7 -arch C FencedWW Rfe DpAddrdW Coe
  Warning: optimised conditions are not supported by C arch
  C S+fencesc+addr
  "FenceScdWW Rfe DpAddrdW Coe"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Co
  Orig=FenceScdWW Rfe DpAddrdW Coe
  
  {}
  
  P0 (volatile int* y,volatile int* x) {
    *x = 2;
    atomic_thread_fence(memory_order_seq_cst);
    *y = 1;
  }
  
  P1 (volatile int* y,volatile int* x) {
    int r0 = *y;
    *(x + (r0 & 128)) = 1;
  }
  
  exists ([x]=2 /\ 1:r0=1)
A C test for forall
  $ diyone7 -arch C FencedWW Sc Rfe Acq PodRW Coe -cond unicond
  Warning: optimised conditions are not supported by C arch
  C S+fencescnasc+poacqna
  "FenceScdWWNaSc RfeScAcq PodRWAcqNa Coe"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Co
  Orig=FenceScdWWNaSc RfeScAcq PodRWAcqNa Coe
  
  {}
  
  P0 (atomic_int* y,volatile int* x) {
    *x = 2;
    atomic_thread_fence(memory_order_seq_cst);
    atomic_store_explicit(y,1,memory_order_seq_cst);
  }
  
  P1 (atomic_int* y,volatile int* x) {
    int r0 = atomic_load_explicit(y,memory_order_acquire);
    *x = 1;
  }
  
  forall (true /\ ([y]=1 /\ ([x]=2 /\ (1:r0=1 \/ 1:r0=0) \/ [x]=1 /\ (1:r0=1 \/ 1:r0=0))))
A valid cycle with duplicate annotations
  $ diyone7 -arch AArch64 PodWR A A Fre PodWR Fre
  AArch64 SB+po+popa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA FreAP PodWR Fre
  "PodWRPA FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   LDAR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
  $ diyone7 -arch AArch64 PodWR A A A Fre PodWR Fre
  AArch64 SB+po+popa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA FreAP PodWR Fre
  "PodWRPA FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   LDAR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
A C test for negated exists
  $ diyone7 -arch C FencedWW Rfe DpAddrdW Coe
  Warning: optimised conditions are not supported by C arch
  C S+fencesc+addr
  "FenceScdWW Rfe DpAddrdW Coe"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Co
  Orig=FenceScdWW Rfe DpAddrdW Coe
  
  {}
  
  P0 (volatile int* y,volatile int* x) {
    *x = 2;
    atomic_thread_fence(memory_order_seq_cst);
    *y = 1;
  }
  
  P1 (volatile int* y,volatile int* x) {
    int r0 = *y;
    *(x + (r0 & 128)) = 1;
  }
  
  exists ([x]=2 /\ 1:r0=1)
A repeated C test for forall
  $ diyone7 -arch C FencedWW Sc Rfe Acq PodRW Coe -cond unicond
  Warning: optimised conditions are not supported by C arch
  C S+fencescnasc+poacqna
  "FenceScdWWNaSc RfeScAcq PodRWAcqNa Coe"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=W,1:y=F,1:x=W
  Com=Rf Co
  Orig=FenceScdWWNaSc RfeScAcq PodRWAcqNa Coe
  
  {}
  
  P0 (atomic_int* y,volatile int* x) {
    *x = 2;
    atomic_thread_fence(memory_order_seq_cst);
    atomic_store_explicit(y,1,memory_order_seq_cst);
  }
  
  P1 (atomic_int* y,volatile int* x) {
    int r0 = atomic_load_explicit(y,memory_order_acquire);
    *x = 1;
  }
  
  forall (true /\ ([y]=1 /\ ([x]=2 /\ (1:r0=1 \/ 1:r0=0) \/ [x]=1 /\ (1:r0=1 \/ 1:r0=0))))
A valid cycle with a default annotation, `P`
  $ diyone7 -arch AArch64 PodWR P Fre PodWR Fre
  AArch64 SB
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  "PodWR Fre PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
A valid cycle with the annotation `L`
  $ diyone7 -arch AArch64 L PodWR Fre PodWR Fre
  AArch64 SB+po+polp
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRLP Fre PodWR FrePL
  "PodWRLP Fre PodWR FrePL"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STLR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2]  | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
Valid cycles with duplicate annotations
  $ diyone7 -arch AArch64 L L PodWR Fre PodWR Fre
  AArch64 SB+po+polp
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRLP Fre PodWR FrePL
  "PodWRLP Fre PodWR FrePL"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STLR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2]  | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
  $ diyone7 -arch AArch64 PodWR A A Fre PodWR Fre
  AArch64 SB+po+popa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA FreAP PodWR Fre
  "PodWRPA FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   LDAR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
  $ diyone7 -arch AArch64 PodWR A A A Fre PodWR Fre
  AArch64 SB+po+popa
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA FreAP PodWR Fre
  "PodWRPA FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   LDAR W3,[X2] | LDR W3,[X1] ;
  
  exists (0:X3=0 /\ 1:X3=0)
An invalid `diyone7` input that expands to several cycles
  $ diyone7 -arch AArch64 'PodWR|Fre'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
  $ diyone7 -arch AArch64 '[PodWR|Fre]'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
  $ diyone7 -arch AArch64 'PodWR?'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
  $ diyone7 -arch AArch64 'PodWR|[Fre,PodWR]'
  diyone7: Fatal error: `diyone7` only accepts exactly one input cycle.
  [2]
Invalid cycles with incorrect annotations
  $ diyone7 -arch AArch64 L PodWR Fre PodWR Fre A
  diyone7: Fatal error: Annotations mismatch between A L.
  [2]
  $ diyone7 -arch AArch64 PodWR A L Fre PodWR Fre
  diyone7: Fatal error: Annotations mismatch between A L.
  [2]
  $ diyone7 -arch AArch64 PodWR L A Fre PodWR Fre
  diyone7: Fatal error: Annotations mismatch between L A.
  [2]
  $ diyone7 -arch AArch64 PodWR L Fre PodWR Fre
  diyone7: Fatal error: Test SB+po+popl [PodWRPL FreLP PodWR Fre] failed:
  annotation mismatch on edge FreLP, annotation 'L' on R
  [2]
  $ diyone7 -arch AArch64 PodWR A L A Fre PodWR Fre
  diyone7: Fatal error: Invalid extra annotation L
  [2]
A valid cycle with annotations and insert edges
  $ diyone7 -arch AArch64 PodWR A ISB P A DMB.SY Fre PodWR Fre
  AArch64 SB+po+popa-[isb]-[dmb.sy]
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA ISB DMB.SY FreAP PodWR Fre
  "PodWRPA ISB DMB.SY FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   ISB          | LDR W3,[X1] ;
   DMB SY       |             ;
   LDAR W3,[X2] |             ;
  
  exists (0:X3=0 /\ 1:X3=0)
A valid cycle with an annotation after an insert edge
  $ diyone7 -arch AArch64 PodWR ISB A Fre PodWR Fre
  AArch64 SB+po+popa-[isb]
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA ISB FreAP PodWR Fre
  "PodWRPA ISB FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   ISB          | LDR W3,[X1] ;
   LDAR W3,[X2] |             ;
  
  exists (0:X3=0 /\ 1:X3=0)
A valid cycle with duplicate annotations around an insert edge
  $ diyone7 -arch AArch64 PodWR A ISB A Fre PodWR Fre
  AArch64 SB+po+popa-[isb]
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA ISB FreAP PodWR Fre
  "PodWRPA ISB FreAP PodWR Fre"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   ISB          | LDR W3,[X1] ;
   LDAR W3,[X2] |             ;
  
  exists (0:X3=0 /\ 1:X3=0)
Invalid cycles with annotations and insert edges
  $ diyone7 -arch AArch64 PodWR A ISB P L A DMB.SY Fre PodWR Fre
  diyone7: Fatal error: Invalid extra annotation L
  [2]
  $ diyone7 -arch AArch64 PodWR L ISB A Fre PodWR Fre
  diyone7: Fatal error: Annotations mismatch between L A.
  [2]
A valid cycle with annotations and a store edge
  $ diyone7 -arch AArch64 L Store PodWR Fre PodWR Fre
  AArch64 SB+po+store-polp
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=Store PodWRLP Fre PodWR FrePL
  "Store PodWRLP Fre PodWR FrePL"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W4,#1    | MOV W0,#1   ;
   STR W4,[X1]  | STR W0,[X2] ;
   MOV W0,#2    | LDR W3,[X1] ;
   STLR W0,[X1] |             ;
   LDR W3,[X2]  |             ;
  
  exists ([x]=2 /\ 0:X3=0 /\ 1:X3=0)
An invalid cycle with annotations and a store edge
  $ diyone7 -arch AArch64 A Store PodWR Fre PodWR Fre
  diyone7: Fatal error: Test SB+po+store-poap [Store PodWRAP Fre PodWR FrePA] failed:
  annotation mismatch on edge PodWRAP, annotation 'A' on W
  [2]
A valid cycle with duplicate wraparound annotations plus insert and store edges
  $ diyone7 -arch AArch64 L L Store PodWR ISB A A Fre PodWR Fre
  AArch64 SB+po+store-pola-[isb]
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=Store PodWRLA ISB FreAP PodWR FrePL
  "Store PodWRLA ISB FreAP PodWR FrePL"
  {
   0:X1=x; 0:X2=y;
   1:X1=x; 1:X2=y;
  }
   P0           | P1          ;
   MOV W4,#1    | MOV W0,#1   ;
   STR W4,[X1]  | STR W0,[X2] ;
   MOV W0,#2    | LDR W3,[X1] ;
   STLR W0,[X1] |             ;
   ISB          |             ;
   LDAR W3,[X2] |             ;
  
  exists ([x]=2 /\ 0:X3=0 /\ 1:X3=0)

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `default` mode
  $ diy7 -arch AArch64 -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `default`

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `free` mode
  $ diy7 -arch AArch64 -mode free -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `free`

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `sc` mode
  $ diy7 -arch AArch64 -mode sc -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `sc`

`diy7 -unfold-only` unfolds relaxations and drops invalid composites
  $ diy7 -arch AArch64 -relax '[Po,DpAddr?]' -unfold-only 2>&1
  ***relax***
  PosWW PosWR [PosWR,DpAddrsW] [PosWR,DpAddrsR] [PosWR,DpAddrdW] [PosWR,DpAddrdR] PosRW PosRR [PosRR,DpAddrsW] [PosRR,DpAddrsR] [PosRR,DpAddrdW] [PosRR,DpAddrdR] PodWW PodWR [PodWR,DpAddrsW] [PodWR,DpAddrsR] [PodWR,DpAddrdW] [PodWR,DpAddrdR] PodRW PodRR [PodRR,DpAddrsW] [PodRR,DpAddrsR] [PodRR,DpAddrdW] [PodRR,DpAddrdR]
  ***safe***
  
  ***reject***
  
`diy7 -unfold-only` expands choice, optional, and grouped syntax
  $ diy7 -arch AArch64 -relax 'PodWR|Fre' -unfold-only 2>&1
  ***relax***
  Fre PodWR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'PodWR?' -unfold-only 2>&1
  ***relax***
  PodWR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax '[PodWR Fre]' -unfold-only 2>&1
  ***relax***
  [PodWR,Fre]
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax 'PodWR Fre' -unfold-only 2>&1
  ***relax***
  Fre PodWR
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax '@after(PodWR|Fre)' -unfold-only 2>&1
  ***relax***
  @after(Fre) @after(PodWR)
  ***safe***
  
  ***reject***
  
  $ diy7 -arch AArch64 -relax '@before(PodWR?)' -unfold-only 2>&1
  ***relax***
  @before(PodWR)
  ***safe***
  
  ***reject***
  

`diy7 -unfold-only` removes duplicate relaxes after unfolding
  $ diy7 -arch AArch64 -relax 'PodWR|PodWR' -unfold-only 2>&1
  ***relax***
  PodWR
  ***safe***
  
  ***reject***
  
`diy7 -unfold-only` also unfolds `-safe`
  $ diy7 -arch AArch64 -safe 'Fre|Coe' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre Coe
  ***reject***
  
  $ diy7 -arch AArch64 -safe 'Fre?' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre
  ***reject***
  
  $ diy7 -arch AArch64 -safe '[PodWR Fre]' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  [PodWR,Fre]
  ***reject***
  
  $ diy7 -arch AArch64 -safe 'Fre Coe' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre Coe
  ***reject***
  
  $ diy7 -arch AArch64 -safe '@after(PodWR|Fre)' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  @after(Fre) @after(PodWR)
  ***reject***
  
  $ diy7 -arch AArch64 -safe '@before(PodWR?)' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  @before(PodWR)
  ***reject***
  

`diy7 -unfold-only` removes duplicate safe edges after unfolding
  $ diy7 -arch AArch64 -safe 'Fre|Fre' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  Fre
  ***reject***
  
`diy7 -unfold-only` also unfolds `-rejectlist`
  $ diy7 -arch AArch64 -rejectlist 'Fre|Coe' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre Coe
  $ diy7 -arch AArch64 -rejectlist 'Fre?' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre
  $ diy7 -arch AArch64 -rejectlist '[PodWR Fre]' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  [PodWR,Fre]
  $ diy7 -arch AArch64 -rejectlist 'Fre Coe' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre Coe
  $ diy7 -arch AArch64 -rejectlist '@after(PodWR|Fre)' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  @after(Fre) @after(PodWR)
  $ diy7 -arch AArch64 -rejectlist '@before(PodWR?)' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  @before(PodWR)

`diy7 -unfold-only` removes duplicate reject edges after unfolding
  $ diy7 -arch AArch64 -rejectlist 'Fre|Fre' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre

`diy7 -unfold-only` accumulates repeated `-rejectlist`
  $ diy7 -arch AArch64 -rejectlist 'PodWW' -rejectlist 'Fre' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre PodWW

`diy7 -reject` is an alias for `-rejectlist`
  $ diy7 -arch AArch64 -reject 'PodWW' -rejectlist 'Fre' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre PodWW

  $ diy7 -arch AArch64 -rejectlist 'PodWW' -rejectlist 'Fre' -rejectlist 'PodWW' -unfold-only 2>&1
  ***relax***
  
  ***safe***
  
  ***reject***
  Fre PodWW
