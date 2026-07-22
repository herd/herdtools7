VMSA single PTE annotations on one location
  $ diyone7 -arch AArch64 -variant vmsa -oneloc Pte PosRW Rfi
  AArch64 CoRW1+posptep-rfippte
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteP RfiPPte
  "PosRWPteP RfiPPte"
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0          ;
   LDR X1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X3] ;
  
  exists (0:X1=(oa:PA(x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteWB PosRW Rfi
  AArch64 CoRW1+posptewbp-rfipptewb
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteWBP RfiPPteWB
  "PosRWPteWBP RfiPPteWB"
  {
   [x]=1;
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteWT PosRW Rfi
  AArch64 CoRW1+posptewtp-rfipptewt
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteWTP RfiPPteWT
  "PosRWPteWTP RfiPPteWT"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT));
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteNC PosRW Rfi
  AArch64 CoRW1+posptencp-rfipptenc
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteNCP RfiPPteNC
  "PosRWPteNCP RfiPPteNC"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Normal,ISH,iNC,oNC));
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteGRE PosRW Rfi
  AArch64 CoRW1+posptegrep-rfipptegre
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteGREP RfiPPteGRE
  "PosRWPteGREP RfiPPteGRE"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Device-GRE));
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PtenGRE PosRW Rfi
  AArch64 CoRW1+posptengrep-rfipptengre
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPtenGREP RfiPPtenGRE
  "PosRWPtenGREP RfiPPtenGRE"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Device-nGRE));
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PtenGnRE PosRW Rfi
  AArch64 CoRW1+posptengnrep-rfipptengnre
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPtenGnREP RfiPPtenGnRE
  "PosRWPtenGnREP RfiPPtenGnRE"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Device-nGnRE));
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PtenGnRnE PosRW Rfi
  AArch64 CoRW1+posptengnrnep-rfipptengnrne
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPtenGnRnEP RfiPPtenGnRnE
  "PosRWPtenGnRnEP RfiPPtenGnRnE"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Device-nGnRnE));
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteOA Rfi
  AArch64 CoRW1+posppteoa-rfipteoap
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteOA RfiPteOAP
  "PosRWPPteOA RfiPteOAP"
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0          ;
   LDR W1,[X0] ;
   STR X3,[X2] ;
  
  exists (true)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteV1 Rfi
  AArch64 CoRW1+pospptev1-rfiptev1p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteV1 RfiPteV1P
  "PosRWPPteV1 RfiPteV1P"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteV0 Rfi
  AArch64 CoRW1+pospptev0-rfiptev0p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteV0 RfiPteV0P
  "PosRWPPteV0 RfiPteV0P"
  {
   [x]=1;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), valid:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteAF1 Rfi
  AArch64 CoRW1+posppteaf1-rfipteaf1p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteAF1 RfiPteAF1P
  "PosRWPPteAF1 RfiPteAF1P"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteAF0 Rfi
  AArch64 CoRW1+posppteaf0-rfipteaf0p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteAF0 RfiPteAF0P
  "PosRWPPteAF0 RfiPteAF0P"
  {
   [x]=1;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteDB1 Rfi
  AArch64 CoRW1+pospptedb1-rfiptedb1p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteDB1 RfiPteDB1P
  "PosRWPPteDB1 RfiPteDB1P"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0          ;
   LDR W1,[X0] ;
   STR X3,[X2] ;
  
  exists (true)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteDB0 Rfi
  AArch64 CoRW1+pospptedb0-rfiptedb0p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteDB0 RfiPteDB0P
  "PosRWPPteDB0 RfiPteDB0P"
  {
   [x]=1;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), db:0);
  }
   P0          ;
   LDR W1,[X0] ;
   STR X3,[X2] ;
  
  exists (true)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteDBM1 Rfi
  AArch64 CoRW1+pospptedbm1-rfiptedbm1p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteDBM1 RfiPteDBM1P
  "PosRWPPteDBM1 RfiPteDBM1P"
  {
   [x]=1;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), dbm:1);
  }
   P0          ;
   LDR W1,[X0] ;
   STR X3,[X2] ;
  
  exists (true)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteDBM0 Rfi
  AArch64 CoRW1+pospptedbm0-rfiptedbm0p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteDBM0 RfiPteDBM0P
  "PosRWPPteDBM0 RfiPteDBM0P"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), dbm:1);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0          ;
   LDR W1,[X0] ;
   STR X3,[X2] ;
  
  exists (true)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteHD Rfi
  AArch64 CoRW1+pospptehd-rfiptehdp
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteHD RfiPteHDP
  "PosRWPPteHD RfiPteHDP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteHA PosRW Rfi
  AArch64 CoRW1+posptehap-rfippteha
  Variant=vmsa
  TTHM=HA
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteHAP RfiPPteHA
  "PosRWPteHAP RfiPPteHA"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))
VMSA combined PTE field annotations on one location
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteAF0 PteDB1 Rfi
  AArch64 CoRW1+pospptedb1.af0-rfiptedb1.af0p
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteDB1.AF0 RfiPteDB1.AF0P
  "PosRWPPteDB1.AF0 RfiPteDB1.AF0P"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

VMSA PTE memory attributes combined with field annotations on one location
  $ diyone7 -arch AArch64 -variant vmsa -oneloc Pte PteWT PosRW Rfi
  AArch64 CoRW1+posptewtp-rfipptewt
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteWTP RfiPPteWT
  "PosRWPteWTP RfiPPteWT"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT));
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0          ;
   LDR X1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X3] ;
  
  exists (0:X1=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteWT PteHA PosRW Rfi
  AArch64 CoRW1+pospteha.wtp-rfippteha.wt
  Variant=vmsa
  TTHM=HA
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteHA.WTP RfiPPteHA.WT
  "PosRWPteHA.WTP RfiPPteHA.WT"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0, attrs:(Normal,ISH,iWT,oWT));
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteWT PteV1 Rfi
  AArch64 CoRW1+pospptev1.wt-rfiptev1.wtp
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteV1.WT RfiPteV1.WTP
  "PosRWPPteV1.WT RfiPteV1.WTP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0, attrs:(Normal,ISH,iWT,oWT));
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteWT PteHD Rfi
  AArch64 CoRW1+pospptehd.wt-rfiptehd.wtp
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteHD.WT RfiPteHD.WTP
  "PosRWPPteHD.WT RfiPteHD.WTP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1, attrs:(Normal,ISH,iWT,oWT));
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

VMSA PTE annotations combined with access annotations on one location
  $ diyone7 -arch AArch64 -variant vmsa -oneloc Pte PteWT A PosRW Rfi
  AArch64 CoRW1+posptewt.ap-rfipptewt.a
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteWT.AP RfiPPteWT.A
  "PosRWPteWT.AP RfiPPteWT.A"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT));
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   MOV W2,#2    ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteHA A PosRW Rfi
  AArch64 CoRW1+pospteha.ap-rfippteha.a
  Variant=vmsa
  TTHM=HA
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteHA.AP RfiPPteHA.A
  "PosRWPteHA.AP RfiPPteHA.A"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0                ;
   L01: LDAR W1,[X0] ;
   MOV W2,#2         ;
   L00: STR W2,[X0]  ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteWT L Rfi
  AArch64 CoRW1+pospptewt.l-rfiptewt.lp
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteWT.L RfiPteWT.LP
  "PosRWPPteWT.L RfiPteWT.LP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT));
   0:X0=x;
  }
   P0           ;
   LDR W1,[X0]  ;
   MOV W2,#2    ;
   STLR W2,[X0] ;
  
  exists (0:X1=2)
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteV1 L Rfi
  AArch64 CoRW1+pospptev1.l-rfiptev1.lp
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteV1.L RfiPteV1.LP
  "PosRWPPteV1.L RfiPteV1.LP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STLR X3,[X2]     ;
  
  exists (not (fault(P0:L00,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteHD L Rfi
  AArch64 CoRW1+pospptehd.l-rfiptehd.lp
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteHD.L RfiPteHD.LP
  "PosRWPPteHD.L RfiPteHD.LP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   0:X0=x;
  }
   P0                ;
   L01: LDR W1,[X0]  ;
   MOV W2,#2         ;
   L00: STLR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

VMSA PTE memory attributes and fields combined with access annotations on one location
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PteWT PteHA A PosRW Rfi
  AArch64 CoRW1+pospteha.wt.ap-rfippteha.wt.a
  Variant=vmsa
  TTHM=HA
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPteHA.WT.AP RfiPPteHA.WT.A
  "PosRWPteHA.WT.AP RfiPPteHA.WT.A"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0, attrs:(Normal,ISH,iWT,oWT));
   0:X0=x;
  }
   P0                ;
   L01: LDAR W1,[X0] ;
   MOV W2,#2         ;
   L00: STR W2,[X0]  ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteWT PteV1 L Rfi
  AArch64 CoRW1+pospptev1.wt.l-rfiptev1.wt.lp
  Variant=vmsa
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteV1.WT.L RfiPteV1.WT.LP
  "PosRWPPteV1.WT.L RfiPteV1.WT.LP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0, attrs:(Normal,ISH,iWT,oWT));
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), attrs:(Normal,ISH,iWT,oWT));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STLR X3,[X2]     ;
  
  exists (not (fault(P0:L00,x)))
  $ diyone7 -arch AArch64 -variant vmsa -oneloc PosRW PteWT PteHD L Rfi
  AArch64 CoRW1+pospptehd.wt.l-rfiptehd.wt.lp
  Variant=vmsa
  TTHM=HD
  Generator=diyone7 (version 7.58+1)
  Com=Rf
  Orig=PosRWPPteHD.WT.L RfiPteHD.WT.LP
  "PosRWPPteHD.WT.L RfiPteHD.WT.LP"
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1, attrs:(Normal,ISH,iWT,oWT));
   0:X0=x;
  }
   P0                ;
   L01: LDR W1,[X0]  ;
   MOV W2,#2         ;
   L00: STLR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))
