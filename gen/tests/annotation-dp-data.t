AArch64 data-dependent annotation compilation checks

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW P Rfi
  AArch64 CoRW1+datas-rfi
  {
   0:X0=x;
  }
   P0           ;
   LDR W1,[X0]  ;
   EOR W2,W1,W1 ;
   ADD W2,W2,#1 ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW A Rfi
  diyone7: Fatal error: Test CoRW1+dataspa-rfiap [DpDatasWPA RfiAP] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc A DpDatasW P Rfi
  AArch64 CoRW1+datasap-rfipa
  {
   0:X0=x;
  }
   P0           ;
   LDAR W1,[X0] ;
   EOR W2,W1,W1 ;
   ADD W2,W2,#1 ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW Q Rfi
  diyone7: Fatal error: Test CoRW1+dataspq-rfiqp [DpDatasWPQ RfiQP] failed:
  No store acquirePc
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc Q DpDatasW P Rfi
  AArch64 CoRW1+datasqp-rfipq
  {
   0:X0=x;
  }
   P0            ;
   LDAPR W1,[X0] ;
   EOR W2,W1,W1  ;
   ADD W2,W2,#1  ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW L Rfi
  AArch64 CoRW1+dataspl-rfilp
  {
   0:X0=x;
  }
   P0           ;
   LDR W1,[X0]  ;
   EOR W2,W1,W1 ;
   ADD W2,W2,#1 ;
   STLR W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc L DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+dataslp-rfipl [DpDatasWLP RfiPL] failed:
  annotation mismatch on edge DpDatasWLP, annotation 'L' on R
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW X Rfi
  AArch64 CoRW1+dataspx-rfixp
  {
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   EOR W2,W1,W1    ;
   ADD W2,W2,#1    ;
   Loop00:         ;
   LDXR W3,[X0]    ;
   STXR W4,W2,[X0] ;
   CBNZ W4,Loop00  ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc X DpDatasW P Rfi
  AArch64 CoRW1+datasxp-rfipx
  {
   0:X0=x;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   EOR W3,W1,W1    ;
   ADD W3,W3,#1    ;
   STR W3,[X0]     ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW XA Rfi
  AArch64 CoRW1+dataspxa-rfixap
  {
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   EOR W2,W1,W1    ;
   ADD W2,W2,#1    ;
   Loop00:         ;
   LDAXR W3,[X0]   ;
   STXR W4,W2,[X0] ;
   CBNZ W4,Loop00  ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XA DpDatasW P Rfi
  AArch64 CoRW1+datasxap-rfipxa
  {
   0:X0=x;
  }
   P0              ;
   Loop00:         ;
   LDAXR W1,[X0]   ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   EOR W3,W1,W1    ;
   ADD W3,W3,#1    ;
   STR W3,[X0]     ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW XL Rfi
  AArch64 CoRW1+dataspxl-rfixlp
  {
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#1     ;
   Loop00:          ;
   LDXR W3,[X0]     ;
   STLXR W4,W2,[X0] ;
   CBNZ W4,Loop00   ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XL DpDatasW P Rfi
  AArch64 CoRW1+datasxlp-rfipxl
  {
   0:X0=x;
  }
   P0               ;
   Loop00:          ;
   LDXR W1,[X0]     ;
   STLXR W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   EOR W3,W1,W1     ;
   ADD W3,W3,#1     ;
   STR W3,[X0]      ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW XAL Rfi
  AArch64 CoRW1+dataspxal-rfixalp
  {
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#1     ;
   Loop00:          ;
   LDAXR W3,[X0]    ;
   STLXR W4,W2,[X0] ;
   CBNZ W4,Loop00   ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XAL DpDatasW P Rfi
  AArch64 CoRW1+datasxalp-rfipxal
  {
   0:X0=x;
  }
   P0               ;
   Loop00:          ;
   LDAXR W1,[X0]    ;
   STLXR W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   EOR W3,W1,W1     ;
   ADD W3,W3,#1     ;
   STR W3,[X0]      ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW Pa Rfi
  AArch64 CoRW1+datasppa-rfipap
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDR W1,[X0]    ;
   EOR W2,W1,W1   ;
   ADD W2,W2,#2   ;
   SUB W3,W2,#1   ;
   STP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa DpDatasW P Rfi
  AArch64 CoRW1+dataspap-rfippa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDP W1,W2,[X0] ;
   ADD W1,W1,W2   ;
   EOR W3,W1,W1   ;
   ADD W3,W3,#1   ;
   STR W3,[X0]    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW PaN Rfi
  AArch64 CoRW1+datasppan-rfipanp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   EOR W2,W1,W1    ;
   ADD W2,W2,#2    ;
   SUB W3,W2,#1    ;
   STNP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc PaN DpDatasW P Rfi
  AArch64 CoRW1+dataspanp-rfippan
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDNP W1,W2,[X0] ;
   ADD W1,W1,W2    ;
   EOR W3,W1,W1    ;
   ADD W3,W3,#1    ;
   STR W3,[X0]     ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW PaIL Rfi
  AArch64 CoRW1+datasppail-rfipailp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#2     ;
   SUB W3,W2,#1     ;
   STILP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc PaIQ DpDatasW P Rfi
  AArch64 CoRW1+dataspaiqp-rfippaiq
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                ;
   LDIAPP W1,W2,[X0] ;
   ADD W1,W1,W2      ;
   EOR W3,W1,W1      ;
   ADD W3,W3,#1      ;
   STR W3,[X0]       ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW PaL Rfi
  AArch64 CoRW1+datasppal-rfipalp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   EOR W2,W1,W1    ;
   ADD W2,W2,#2    ;
   SUB W3,W2,#1    ;
   STLP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpDatasW Pa L Rfi
  diyone7: Fatal error: Annotations mismatch between Pa L.
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaA DpDatasW P Rfi
  AArch64 CoRW1+dataspaap-rfippaa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDAP W1,W2,[X0] ;
   ADD W1,W1,W2    ;
   EOR W3,W1,W1    ;
   ADD W3,W3,#1    ;
   STR W3,[X0]     ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa A DpDatasW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pa
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW A.b0 Rfi
  diyone7: Fatal error: Test CoRW1+datasw0a.b0-rfia.b0w0 [DpDatasWw0A.b0 RfiA.b0w0] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.b0 DpDatasW P Rfi
  AArch64 CoRW1+datasa.b0w0-rfiw0a.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0            ;
   LDARB W1,[X0] ;
   EOR W2,W1,W1  ;
   ADD W2,W2,W3  ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 A DpDatasW P Rfi
  AArch64 CoRW1+datasa.b0w0-rfiw0a.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0            ;
   LDARB W1,[X0] ;
   EOR W2,W1,W1  ;
   ADD W2,W2,W3  ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW A.h0 Rfi
  diyone7: Fatal error: Test CoRW1+datasw0a.h0-rfia.h0w0 [DpDatasWw0A.h0 RfiA.h0w0] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.h0 DpDatasW P Rfi
  AArch64 CoRW1+datasa.h0w0-rfiw0a.h0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0            ;
   LDARH W1,[X0] ;
   EOR W2,W1,W1  ;
   ADD W2,W2,W3  ;
   STR W2,[X0]   ;
  
  exists (0:X1=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW A.w0 Rfi
  diyone7: Fatal error: Test CoRW1+datasw0a.w0-rfia.w0w0 [DpDatasWw0A.w0 RfiA.w0w0] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.w0 DpDatasW P Rfi
  AArch64 CoRW1+datasa.w0w0-rfiw0a.w0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0           ;
   LDAR W1,[X0] ;
   EOR W2,W1,W1 ;
   ADD W2,W2,W3 ;
   STR W2,[X0]  ;
  
  exists (0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW L.b0 Rfi
  AArch64 CoRW1+datasw0l.b0-rfil.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   MOV W3,#1     ;
   EOR W2,W0,W0  ;
   ADD W2,W2,W3  ;
   STLRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW L b0 Rfi
  AArch64 CoRW1+datasw0l.b0-rfil.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   MOV W3,#1     ;
   EOR W2,W0,W0  ;
   ADD W2,W2,W3  ;
   STLRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.b0 DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+datasl.b0w0-rfiw0l.b0 [DpDatasWL.b0w0 Rfiw0L.b0] failed:
  annotation mismatch on edge DpDatasWL.b0w0, annotation 'L.b0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW L.h0 Rfi
  AArch64 CoRW1+datasw0l.h0-rfil.h0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   MOV W3,#257   ;
   EOR W2,W0,W0  ;
   ADD W2,W2,W3  ;
   STLRH W2,[X1] ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.h0 DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+datasl.h0w0-rfiw0l.h0 [DpDatasWL.h0w0 Rfiw0L.h0] failed:
  annotation mismatch on edge DpDatasWL.h0w0, annotation 'L.h0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW L.w0 Rfi
  AArch64 CoRW1+datasw0l.w0-rfil.w0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STLR W2,[X1] ;
  
  exists (0:X0=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.w0 DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+datasl.w0w0-rfiw0l.w0 [DpDatasWL.w0w0 Rfiw0L.w0] failed:
  annotation mismatch on edge DpDatasWL.w0w0, annotation 'L.w0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW X.b0 Rfi
  AArch64 CoRW1+datasw0x.b0-rfix.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0               ;
   LDR W0,[X1]      ;
   MOV W3,#1        ;
   EOR W2,W0,W0     ;
   ADD W2,W2,W3     ;
   Loop00:          ;
   LDXRB W4,[X1]    ;
   STXRB W5,W2,[X1] ;
   CBNZ W5,Loop00   ;
  
  exists ([x]=1 /\ 0:X0=1 /\ 0:X4=0)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.b0 DpDatasW P Rfi
  AArch64 CoRW1+datasx.b0w0-rfiw0x.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X4=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXRB W1,[X0]    ;
   STXRB W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   EOR W3,W1,W1     ;
   ADD W3,W3,W4     ;
   STR W3,[X0]      ;
  
  exists ([x]=16843009 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW X.h0 Rfi
  AArch64 CoRW1+datasw0x.h0-rfix.h0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0               ;
   LDR W0,[X1]      ;
   MOV W3,#257      ;
   EOR W2,W0,W0     ;
   ADD W2,W2,W3     ;
   Loop00:          ;
   LDXRH W4,[X1]    ;
   STXRH W5,W2,[X1] ;
   CBNZ W5,Loop00   ;
  
  exists ([x]=257 /\ 0:X0=257 /\ 0:X4=256)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.h0 DpDatasW P Rfi
  AArch64 CoRW1+datasx.h0w0-rfiw0x.h0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X4=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXRH W1,[X0]    ;
   STXRH W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   EOR W3,W1,W1     ;
   ADD W3,W3,W4     ;
   STR W3,[X0]      ;
  
  exists ([x]=16843009 /\ 0:X1=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW X.w0 Rfi
  AArch64 CoRW1+datasw0x.w0-rfix.w0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0              ;
   LDR W0,[X1]     ;
   EOR W2,W0,W0    ;
   ADD W2,W2,W3    ;
   Loop00:         ;
   LDXR W4,[X1]    ;
   STXR W5,W2,[X1] ;
   CBNZ W5,Loop00  ;
  
  exists ([x]=16843009 /\ 0:X0=16843009 /\ 0:X4=16843008)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.w0 DpDatasW P Rfi
  AArch64 CoRW1+datasx.w0w0-rfiw0x.w0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X4=16843009;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   EOR W3,W1,W1    ;
   ADD W3,W3,W4    ;
   STR W3,[X0]     ;
  
  exists ([x]=16843009 /\ 0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW b0 Rfi
  AArch64 CoRW1+datasw0b0-rfib0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0           ;
   LDR W0,[X1]  ;
   MOV W3,#1    ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 DpDatasW P Rfi
  AArch64 CoRW1+datasb0w0-rfiw0b0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0           ;
   LDRB W0,[X1] ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STR W2,[X1]  ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW b1 Rfi
  AArch64 CoRW1+datasw0b1-rfib1w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0              ;
   LDR W0,[X1]     ;
   MOV W3,#1       ;
   EOR W2,W0,W0    ;
   ADD W2,W2,W3    ;
   STRB W2,[X1,#1] ;
  
  exists (0:X0=256)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b1 DpDatasW P Rfi
  AArch64 CoRW1+datasb1w0-rfiw0b1
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0              ;
   LDRB W0,[X1,#1] ;
   EOR W2,W0,W0    ;
   ADD W2,W2,W3    ;
   STR W2,[X1]     ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P DpDatasW h0 Rfi
  AArch64 CoRW1+datasw0h0-rfih0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0           ;
   LDR W0,[X1]  ;
   MOV W3,#257  ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STRH W2,[X1] ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc h0 DpDatasW P Rfi
  AArch64 CoRW1+datash0w0-rfiw0h0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0           ;
   LDRH W0,[X1] ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STR W2,[X1]  ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpDatasW h2 Rfi
  AArch64 CoRW1+datasw0h2-rfih2w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=0;
  }
   P0              ;
   LDR W0,[X1]     ;
   MOV W3,#257     ;
   EOR W2,W0,W0    ;
   ADD W2,W2,W3    ;
   STRH W2,[X1,#2] ;
  
  exists (0:X0=16842752)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc h2 DpDatasW P Rfi
  AArch64 CoRW1+datash2w0-rfiw0h2
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0              ;
   LDRH W0,[X1,#2] ;
   EOR W2,W0,W0    ;
   ADD W2,W2,W3    ;
   STR W2,[X1]     ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P DpDatasW w0 Rfi
  AArch64 CoRW1+datasw0w0-rfiw0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STR W2,[X1]  ;
  
  exists (0:X0=16843009)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc w0 DpDatasW P Rfi
  AArch64 CoRW1+datasw0w0-rfiw0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X3=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   EOR W2,W0,W0 ;
   ADD W2,W2,W3 ;
   STR W2,[X1]  ;
  
  exists (0:X0=16843009)

# SIMD annotations

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW NeP Rfi
  AArch64 CoRW1+dataspnep-rfinepp
  Variant=neon
  {
   0:X0=x;
  }
   P0                    ;
   LDR W1,[X0]           ;
   EOR W2,W1,W1          ;
   DUP V0.4S,W2          ;
   MOVI V1.4S,#1         ;
   ADD V1.4S,V1.4S,V0.4S ;
   STUR S1,[X0]          ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP DpDatasW P Rfi
  AArch64 CoRW1+datasnepp-rfipnep
  Variant=neon
  {
   0:X0=x;
  }
   P0           ;
   LDUR S0,[X0] ;
   FMOV W1,S0   ;
   EOR W2,W1,W1 ;
   ADD W2,W2,#1 ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW NeQ Rfi
  diyone7: Fatal error: Test CoRW1+dataspneq-rfineqp [DpDatasWPNeQ RfiNeQP] failed:
  No store acquirePc
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeQ DpDatasW P Rfi
  AArch64 CoRW1+datasneqp-rfipneq
  Variant=neon
  {
   0:X0=x;
  }
   P0             ;
   LDAPUR S0,[X0] ;
   FMOV W1,S0     ;
   EOR W2,W1,W1   ;
   ADD W2,W2,#1   ;
   STR W2,[X0]    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP Q DpDatasW P Rfi
  diyone7: Fatal error: Invalid extra annotation NeP
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW NeL Rfi
  AArch64 CoRW1+dataspnel-rfinelp
  Variant=neon
  {
   0:X0=x;
  }
   P0                    ;
   LDR W1,[X0]           ;
   EOR W2,W1,W1          ;
   DUP V0.4S,W2          ;
   MOVI V1.4S,#1         ;
   ADD V1.4S,V1.4S,V0.4S ;
   STLUR S1,[X0]         ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW L NeP Rfi
  diyone7: Fatal error: Annotations mismatch between L NeP.
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeL DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+datasnelp-rfipnel [DpDatasWNeLP RfiPNeL] failed:
  annotation mismatch on edge DpDatasWNeLP, annotation 'NeL' on R
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW NePa Rfi
  AArch64 CoRW1+dataspnepa-rfinepap
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDR W1,[X0]           ;
   EOR W2,W1,W1          ;
   DUP V0.4S,W2          ;
   MOVI V1.4S,#1         ;
   MOVI V2.4S,#2         ;
   ADD V1.4S,V1.4S,V0.4S ;
   ADD V2.4S,V2.4S,V0.4S ;
   STP S1,S2,[X0]        ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePa DpDatasW P Rfi
  AArch64 CoRW1+datasnepap-rfipnepa
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDP S0,S1,[X0]        ;
   ADD V2.4S,V0.4S,V1.4S ;
   FMOV W1,S2            ;
   EOR W2,W1,W1          ;
   ADD W2,W2,#1          ;
   STR W2,[X0]           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW NePaN Rfi
  AArch64 CoRW1+dataspnepan-rfinepanp
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDR W1,[X0]           ;
   EOR W2,W1,W1          ;
   DUP V0.4S,W2          ;
   MOVI V1.4S,#1         ;
   MOVI V2.4S,#2         ;
   ADD V1.4S,V1.4S,V0.4S ;
   ADD V2.4S,V2.4S,V0.4S ;
   STNP S1,S2,[X0]       ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePaN DpDatasW P Rfi
  AArch64 CoRW1+datasnepanp-rfipnepan
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDNP S0,S1,[X0]       ;
   ADD V2.4S,V0.4S,V1.4S ;
   FMOV W1,S2            ;
   EOR W2,W1,W1          ;
   ADD W2,W2,#1          ;
   STR W2,[X0]           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne1 Rfi
  AArch64 CoRW1+dataspne1-rfine1p
  Variant=neon
  {
   int x[4]={0,0,0,0};
   0:X0=x;
  }
   P0                    ;
   LDR W1,[X0]           ;
   EOR W2,W1,W1          ;
   DUP V1.4S,W2          ;
   MOVI V0.4S,#1         ;
   ADD V0.4S,V0.4S,V1.4S ;
   ST1 {V0.4S},[X0]      ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne1 DpDatasW P Rfi
  AArch64 CoRW1+datasne1p-rfipne1
  Variant=neon
  {
   int x[4]={0,0,0,0};
   0:X0=x;
  }
   P0               ;
   LD1 {V0.4S},[X0] ;
   ADDV S1,V0.4S    ;
   FMOV W1,S1       ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#1     ;
   STR W2,[X0]      ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne2 Rfi
  AArch64 CoRW1+dataspne2-rfine2p
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LDR W1,[X0]             ;
   EOR W2,W1,W1            ;
   DUP V2.4S,W2            ;
   MOVI V0.4S,#1           ;
   MOVI V1.4S,#2           ;
   ADD V0.4S,V0.4S,V2.4S   ;
   ADD V1.4S,V1.4S,V2.4S   ;
   ST1 {V0.4S, V1.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2 DpDatasW P Rfi
  AArch64 CoRW1+datasne2p-rfipne2
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LD1 {V0.4S, V1.4S},[X0] ;
   ADD V0.4S,V0.4S,V1.4S   ;
   ADDV S2,V0.4S           ;
   FMOV W1,S2              ;
   EOR W2,W1,W1            ;
   ADD W2,W2,#1            ;
   STR W2,[X0]             ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne3 Rfi
  AArch64 CoRW1+dataspne3-rfine3p
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LDR W1,[X0]                    ;
   EOR W2,W1,W1                   ;
   DUP V3.4S,W2                   ;
   MOVI V0.4S,#1                  ;
   MOVI V1.4S,#2                  ;
   MOVI V2.4S,#3                  ;
   ADD V0.4S,V0.4S,V3.4S          ;
   ADD V1.4S,V1.4S,V3.4S          ;
   ADD V2.4S,V2.4S,V3.4S          ;
   ST1 {V0.4S, V1.4S, V2.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3 DpDatasW P Rfi
  AArch64 CoRW1+datasne3p-rfipne3
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LD1 {V0.4S, V1.4S, V2.4S},[X0] ;
   ADD V0.4S,V0.4S,V1.4S          ;
   ADD V0.4S,V0.4S,V2.4S          ;
   ADDV S3,V0.4S                  ;
   FMOV W1,S3                     ;
   EOR W2,W1,W1                   ;
   ADD W2,W2,#1                   ;
   STR W2,[X0]                    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne4 Rfi
  AArch64 CoRW1+dataspne4-rfine4p
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LDR W1,[X0]                           ;
   EOR W2,W1,W1                          ;
   DUP V4.4S,W2                          ;
   MOVI V0.4S,#1                         ;
   MOVI V1.4S,#2                         ;
   MOVI V2.4S,#3                         ;
   MOVI V3.4S,#4                         ;
   ADD V0.4S,V0.4S,V4.4S                 ;
   ADD V1.4S,V1.4S,V4.4S                 ;
   ADD V2.4S,V2.4S,V4.4S                 ;
   ADD V3.4S,V3.4S,V4.4S                 ;
   ST1 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4 DpDatasW P Rfi
  AArch64 CoRW1+datasne4p-rfipne4
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LD1 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
   ADD V0.4S,V0.4S,V1.4S                 ;
   ADD V0.4S,V0.4S,V2.4S                 ;
   ADD V0.4S,V0.4S,V3.4S                 ;
   ADDV S4,V0.4S                         ;
   FMOV W1,S4                            ;
   EOR W2,W1,W1                          ;
   ADD W2,W2,#1                          ;
   STR W2,[X0]                           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne2i Rfi
  AArch64 CoRW1+dataspne2i-rfine2ip
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LDR W1,[X0]             ;
   EOR W2,W1,W1            ;
   DUP V2.4S,W2            ;
   MOVI V0.4S,#1           ;
   MOVI V1.4S,#2           ;
   ADD V0.4S,V0.4S,V2.4S   ;
   ADD V1.4S,V1.4S,V2.4S   ;
   ST2 {V0.4S, V1.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2i DpDatasW P Rfi
  AArch64 CoRW1+datasne2ip-rfipne2i
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LD2 {V0.4S, V1.4S},[X0] ;
   ADD V0.4S,V0.4S,V1.4S   ;
   ADDV S2,V0.4S           ;
   FMOV W1,S2              ;
   EOR W2,W1,W1            ;
   ADD W2,W2,#1            ;
   STR W2,[X0]             ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne3i Rfi
  AArch64 CoRW1+dataspne3i-rfine3ip
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LDR W1,[X0]                    ;
   EOR W2,W1,W1                   ;
   DUP V3.4S,W2                   ;
   MOVI V0.4S,#1                  ;
   MOVI V1.4S,#2                  ;
   MOVI V2.4S,#3                  ;
   ADD V0.4S,V0.4S,V3.4S          ;
   ADD V1.4S,V1.4S,V3.4S          ;
   ADD V2.4S,V2.4S,V3.4S          ;
   ST3 {V0.4S, V1.4S, V2.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3i DpDatasW P Rfi
  AArch64 CoRW1+datasne3ip-rfipne3i
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LD3 {V0.4S, V1.4S, V2.4S},[X0] ;
   ADD V0.4S,V0.4S,V1.4S          ;
   ADD V0.4S,V0.4S,V2.4S          ;
   ADDV S3,V0.4S                  ;
   FMOV W1,S3                     ;
   EOR W2,W1,W1                   ;
   ADD W2,W2,#1                   ;
   STR W2,[X0]                    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpDatasW Ne4i Rfi
  AArch64 CoRW1+dataspne4i-rfine4ip
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LDR W1,[X0]                           ;
   EOR W2,W1,W1                          ;
   DUP V4.4S,W2                          ;
   MOVI V0.4S,#1                         ;
   MOVI V1.4S,#2                         ;
   MOVI V2.4S,#3                         ;
   MOVI V3.4S,#4                         ;
   ADD V0.4S,V0.4S,V4.4S                 ;
   ADD V1.4S,V1.4S,V4.4S                 ;
   ADD V2.4S,V2.4S,V4.4S                 ;
   ADD V3.4S,V3.4S,V4.4S                 ;
   ST4 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4i DpDatasW P Rfi
  AArch64 CoRW1+datasne4ip-rfipne4i
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LD4 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
   ADD V0.4S,V0.4S,V1.4S                 ;
   ADD V0.4S,V0.4S,V2.4S                 ;
   ADD V0.4S,V0.4S,V3.4S                 ;
   ADDV S4,V0.4S                         ;
   FMOV W1,S4                            ;
   EOR W2,W1,W1                          ;
   ADD W2,W2,#1                          ;
   STR W2,[X0]                           ;
  
  exists (0:X1=1)

# Morello annotations

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpDatasW Pc Rfi
  AArch64 CoRW1+datasppc-rfipcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1; __uint128 0:X3=0;
  }
   P0           ;
   LDR X1,[C0]  ;
   MOV X3,#1    ;
   EOR X2,X1,X1 ;
   ADD C2,C2,C3 ;
   STR C2,[C0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc DpDatasW P Rfi
  AArch64 CoRW1+dataspcp-rfippc
  Variant=morello
  {
   __uint128 x=0;
   __uint128 0:X0=0; 0:X1=0xffffc0000:x:1;
  }
   P0            ;
   LDR C0,[C1]   ;
   GCVALUE X0,C0 ;
   EOR X2,X0,X0  ;
   ADD X2,X2,#1  ;
   STR X2,[C1]   ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpDatasW Ac Rfi
  diyone7: Fatal error: Test CoRW1+dataspac-rfiacp [DpDatasWPAc RfiAcP] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ac DpDatasW P Rfi
  AArch64 CoRW1+datasacp-rfipac
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0            ;
   LDAR C1,[C0]  ;
   GCVALUE X1,C1 ;
   EOR X2,X1,X1  ;
   ADD X2,X2,#1  ;
   STR X2,[C0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc A DpDatasW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pc
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpDatasW Lc Rfi
  AArch64 CoRW1+datasplc-rfilcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1; __uint128 0:X3=0;
  }
   P0           ;
   LDR X1,[C0]  ;
   MOV X3,#1    ;
   EOR X2,X1,X1 ;
   ADD C2,C2,C3 ;
   STLR C2,[C0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpDatasW L Pc Rfi
  diyone7: Fatal error: Annotations mismatch between L Pc.
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Lc DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+dataslcp-rfiplc [DpDatasWLcP RfiPLc] failed:
  annotation mismatch on edge DpDatasWLcP, annotation 'Lc' on R
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpDatasW Ct Rfi
  AArch64 CoRW1+dataspct-rfictp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0           ;
   LDR X1,[C0]  ;
   EOR X2,X1,X1 ;
   ADD X2,X2,#1 ;
   STCT X2,[X0] ;
  
  exists (0:X1=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ct DpDatasW P Rfi
  AArch64 CoRW1+datasctp-rfipct
  Variant=morello
  {
   __uint128 x=0;
   0:X1=0xffffc0000:x:1;
  }
   P0           ;
   LDCT X0,[X1] ;
   EOR X2,X0,X0 ;
   ADD X2,X2,#1 ;
   STR X2,[C1]  ;
  
  exists (0:X0=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpDatasW Cs Rfi
  AArch64 CoRW1+dataspcs-rficsp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0               ;
   LDR X1,[C0]      ;
   EOR X2,X1,X1     ;
   ADD X2,X2,#0     ;
   MOV X3,#1        ;
   SCVALUE C2,C0,X2 ;
   SEAL C2,C2,C3    ;
   STR C2,[C0]      ;
  
  exists (0:X1=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Cs DpDatasW P Rfi
  AArch64 CoRW1+datascsp-rfipcs
  Variant=morello
  {
   __uint128 x=0;
   __uint128 0:X0=0; 0:X1=0xffffc0000:x:1;
  }
   P0           ;
   LDR C0,[C1]  ;
   GCTYPE X0,C0 ;
   EOR X2,X0,X0 ;
   ADD X2,X2,#1 ;
   STR X2,[C1]  ;
  
  exists (0:X0=0)

# Ifetch annotations

  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc P DpDatasW I Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc I DpDatasW P Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

# MemTag annotations

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc P DpDatasW T Rfi
  AArch64 CoRW1+dataspt-rfitp
  Variant=memtag
  {
   0:X0=x:red; 0:X4=x:green;
  }
   P0                ;
   L00: LDR W1,[X0]  ;
   EOR W2,W1,W1      ;
   ADD X3,X0,W2,SXTW ;
   STG X3,[X4]       ;
  
  exists (0:X1=0 /\ not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc T DpDatasW P Rfi
  AArch64 CoRW1+datastp-rfipt
  Variant=memtag
  {
   0:X1=x:green;
  }
   P0           ;
   MOV X0,X1    ;
   LDG X0,[X1]  ;
   EOR X2,X0,X0 ;
   ADD W2,W2,#1 ;
   STR W2,[X1]  ;
  
  exists (0:X0=x:green)

# VMSA annotations

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW Pte Rfi
  diyone7: Fatal error: Test CoRW1+datasppte-rfiptep [DpDatasWPPte RfiPteP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte DpDatasW P Rfi
  AArch64 CoRW1+datasptep-rfippte
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDR X1,[X0]  ;
   EOR X2,X1,X1 ;
   ADD W2,W2,#2 ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW Pte A Rfi
  diyone7: Fatal error: Test CoRW1+dataspptea-rfipteap [DpDatasWPPteA RfiPteAP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte A DpDatasW P Rfi
  AArch64 CoRW1+dataspteap-rfipptea
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   EOR X2,X1,X1 ;
   ADD W2,W2,#2 ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW Pte Q Rfi
  diyone7: Fatal error: Test CoRW1+datasppteq-rfipteqp [DpDatasWPPteQ RfiPteQP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte Q DpDatasW P Rfi
  AArch64 CoRW1+dataspteqp-rfippteq
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0            ;
   LDAPR X1,[X0] ;
   EOR X2,X1,X1  ;
   ADD W2,W2,#2  ;
   STR W2,[X3]   ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteOA Rfi
  AArch64 CoRW1+datasppteoa-rfipteoap
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X3=(oa:PA(y)); 0:X5=PTE(x);
  }
   P0           ;
   LDR W1,[X0]  ;
   SXTW X4,W1   ;
   EOR X2,X4,X4 ;
   ADD X2,X2,X3 ;
   STR X2,[X5]  ;
  
  exists (true)

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteOA DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+dataspteoap-rfippteoa [DpDatasWPteOAP RfiPPteOA] failed:
  annotation mismatch on edge DpDatasWPteOAP, annotation 'PteOA' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteV1 Rfi
  AArch64 CoRW1+dataspptev1-rfiptev1p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X3=(oa:PA(x)); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STR X2,[X5]      ;
  
  exists (not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteV1 DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+datasptev1p-rfipptev1 [DpDatasWPteV1P RfiPPteV1] failed:
  annotation mismatch on edge DpDatasWPteV1P, annotation 'PteV1' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteAF0 Rfi
  AArch64 CoRW1+datasppteaf0-rfipteaf0p
  Variant=vmsa
  {
   [x]=1;
   0:X0=x; 0:X3=(oa:PA(x), af:0); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STR X2,[X5]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteAF0 DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+dataspteaf0p-rfippteaf0 [DpDatasWPteAF0P RfiPPteAF0] failed:
  annotation mismatch on edge DpDatasWPteAF0P, annotation 'PteAF0' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteHA Rfi
  AArch64 CoRW1+datasppteha-rfiptehap
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#2     ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHA DpDatasW P Rfi
  AArch64 CoRW1+datasptehap-rfippteha
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   EOR X2,X1,X1     ;
   ADD W2,W2,#2     ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteHD Rfi
  AArch64 CoRW1+dataspptehd-rfiptehdp
  Variant=vmsa
  TTHM=HD
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#2     ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHD DpDatasW P Rfi
  diyone7: Fatal error: Test CoRW1+datasptehdp-rfipptehd [DpDatasWPteHDP RfiPPteHD] failed:
  annotation mismatch on edge DpDatasWPteHDP, annotation 'PteHD' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteV1 PteAF0 Rfi
  AArch64 CoRW1+dataspptev1.af0-rfiptev1.af0p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X3=(oa:PA(x), af:0); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STR X2,[X5]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteV1 PteOA Rfi
  AArch64 CoRW1+datasppteoa.v1-rfipteoa.v1p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   [y]=5;
   0:X0=x; 0:X3=(oa:PA(y)); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STR X2,[X5]      ;
  
  exists (not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteAF0 PteOA Rfi
  AArch64 CoRW1+datasppteoa.af0-rfipteoa.af0p
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X3=(oa:PA(y), af:0); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STR X2,[X5]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteV1 PteAF0 PteOA Rfi
  AArch64 CoRW1+datasppteoa.v1.af0-rfipteoa.v1.af0p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   [y]=5;
   0:X0=x; 0:X3=(oa:PA(y), af:0); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STR X2,[X5]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW PteHA PteHD Rfi
  AArch64 CoRW1+datasppteha.hd-rfipteha.hdp
  Variant=vmsa
  TTHM=HA HD
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0, db:0, dbm:1);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   EOR W2,W1,W1     ;
   ADD W2,W2,#2     ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ fault(P0:L00,x) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A Pte DpDatasW P Rfi
  AArch64 CoRW1+dataspteap-rfipptea
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   EOR X2,X1,X1 ;
   ADD W2,W2,#2 ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A PteHA DpDatasW P Rfi
  AArch64 CoRW1+datasptehaap-rfipptehaa
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0                ;
   L01: LDAR W1,[X0] ;
   EOR X2,X1,X1      ;
   ADD W2,W2,#2      ;
   L00: STR W2,[X0]  ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW L PteOA Rfi
  AArch64 CoRW1+datasppteoal-rfipteoalp
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X3=(oa:PA(y)); 0:X5=PTE(x);
  }
   P0           ;
   LDR W1,[X0]  ;
   SXTW X4,W1   ;
   EOR X2,X4,X4 ;
   ADD X2,X2,X3 ;
   STLR X2,[X5] ;
  
  exists (true)

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpDatasW L PteV1 PteAF0 Rfi
  AArch64 CoRW1+dataspptev1.af0l-rfiptev1.af0lp
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X3=(oa:PA(x), af:0); 0:X5=PTE(x);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   SXTW X4,W1       ;
   EOR X2,X4,X4     ;
   ADD X2,X2,X3     ;
   STLR X2,[X5]     ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte X DpDatasW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pte
  [2]
