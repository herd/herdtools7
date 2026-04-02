vmsa-neg-exists
  $ diyone7 -arch AArch64 -variant kvm Amo.Cas TLBI-sync.ISHdWW PteV1 PteAF0 PteOA Rfe Pte PodRW PteHD Rfe -neg true -info "User-define=User-define"
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
vmsa-forall
  $ diyone7 -arch AArch64 -variant kvm Amo.Cas TLBI-sync.ISHdWW PteV1 PteAF0 PteOA Rfe Pte PodRW PteHD Rfe -info "User-define=User-define" -cond observe
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
vmsa-location
  $ diyone7 -arch AArch64 -variant kvm Amo.Cas TLBI-sync.ISHdWW PteV1 PteAF0 PteOA Rfe Pte PodRW PteHD Rfe -info "User-define=User-define" -cond unicond
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
memtag
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
ifetch
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
base-int64-and-array
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
C-exists
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
C-neg-exists
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
C-forall
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
