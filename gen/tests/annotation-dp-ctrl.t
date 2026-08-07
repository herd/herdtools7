AArch64 control-dependent annotation compilation checks

# Baseline annotations

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW P Rfi
  AArch64 CoRW1+ctrls-rfi
  {
   0:X0=x;
  }
   P0           ;
   LDR W1,[X0]  ;
   CBNZ W1,LC00 ;
   LC00:        ;
   MOV W2,#1    ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW A Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspa-rfiap [DpCtrlsWPA RfiAP] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc A DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsap-rfipa
  {
   0:X0=x;
  }
   P0           ;
   LDAR W1,[X0] ;
   CBNZ W1,LC00 ;
   LC00:        ;
   MOV W2,#1    ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW Q Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspq-rfiqp [DpCtrlsWPQ RfiQP] failed:
  No store acquirePc
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc Q DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsqp-rfipq
  {
   0:X0=x;
  }
   P0            ;
   LDAPR W1,[X0] ;
   CBNZ W1,LC00  ;
   LC00:         ;
   MOV W2,#1     ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW L Rfi
  AArch64 CoRW1+ctrlspl-rfilp
  {
   0:X0=x;
  }
   P0           ;
   LDR W1,[X0]  ;
   CBNZ W1,LC00 ;
   LC00:        ;
   MOV W2,#1    ;
   STLR W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc L DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlslp-rfipl [DpCtrlsWLP RfiPL] failed:
  annotation mismatch on edge DpCtrlsWLP, annotation 'L' on R
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW X Rfi
  AArch64 CoRW1+ctrlspx-rfixp
  {
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOV W2,#1       ;
   Loop01:         ;
   LDXR W3,[X0]    ;
   STXR W4,W2,[X0] ;
   CBNZ W4,Loop01  ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc X DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsxp-rfipx
  {
   0:X0=x;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   CBNZ W1,LC01    ;
   LC01:           ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW XA Rfi
  AArch64 CoRW1+ctrlspxa-rfixap
  {
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOV W2,#1       ;
   Loop01:         ;
   LDAXR W3,[X0]   ;
   STXR W4,W2,[X0] ;
   CBNZ W4,Loop01  ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XA DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsxap-rfipxa
  {
   0:X0=x;
  }
   P0              ;
   Loop00:         ;
   LDAXR W1,[X0]   ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   CBNZ W1,LC01    ;
   LC01:           ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW XL Rfi
  AArch64 CoRW1+ctrlspxl-rfixlp
  {
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#1        ;
   Loop01:          ;
   LDXR W3,[X0]     ;
   STLXR W4,W2,[X0] ;
   CBNZ W4,Loop01   ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XL DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsxlp-rfipxl
  {
   0:X0=x;
  }
   P0               ;
   Loop00:          ;
   LDXR W1,[X0]     ;
   STLXR W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   CBNZ W1,LC01     ;
   LC01:            ;
   MOV W3,#1        ;
   STR W3,[X0]      ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW XAL Rfi
  AArch64 CoRW1+ctrlspxal-rfixalp
  {
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#1        ;
   Loop01:          ;
   LDAXR W3,[X0]    ;
   STLXR W4,W2,[X0] ;
   CBNZ W4,Loop01   ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XAL DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsxalp-rfipxal
  {
   0:X0=x;
  }
   P0               ;
   Loop00:          ;
   LDAXR W1,[X0]    ;
   STLXR W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   CBNZ W1,LC01     ;
   LC01:            ;
   MOV W3,#1        ;
   STR W3,[X0]      ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW Pa Rfi
  AArch64 CoRW1+ctrlsppa-rfipap
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDR W1,[X0]    ;
   CBNZ W1,LC00   ;
   LC00:          ;
   MOV W2,#2      ;
   SUB W3,W2,#1   ;
   STP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspap-rfippa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDP W1,W2,[X0] ;
   ADD W1,W1,W2   ;
   CBNZ W1,LC00   ;
   LC00:          ;
   MOV W3,#1      ;
   STR W3,[X0]    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW PaN Rfi
  AArch64 CoRW1+ctrlsppan-rfipanp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOV W2,#2       ;
   SUB W3,W2,#1    ;
   STNP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc PaN DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspanp-rfippan
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDNP W1,W2,[X0] ;
   ADD W1,W1,W2    ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW PaIL Rfi
  AArch64 CoRW1+ctrlsppail-rfipailp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#2        ;
   SUB W3,W2,#1     ;
   STILP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc PaIQ DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspaiqp-rfippaiq
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                ;
   LDIAPP W1,W2,[X0] ;
   ADD W1,W1,W2      ;
   CBNZ W1,LC00      ;
   LC00:             ;
   MOV W3,#1         ;
   STR W3,[X0]       ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW PaL Rfi
  AArch64 CoRW1+ctrlsppal-rfipalp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOV W2,#2       ;
   SUB W3,W2,#1    ;
   STLP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpCtrlsW Pa L Rfi
  diyone7: Fatal error: Annotations mismatch between Pa L.
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaA DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspaap-rfippaa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDAP W1,W2,[X0] ;
   ADD W1,W1,W2    ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa A DpCtrlsW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pa
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaA DpAddrsW P Rfi
  AArch64 CoRW1+addrspaap-rfippaa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                  ;
   LDAP W1,W2,[X0]     ;
   ADD W1,W1,W2        ;
   EOR W3,W1,W1        ;
   MOV W4,#1           ;
   STR W4,[X0,W3,SXTW] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa A DpAddrsW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pa
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

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpAddrsW PaL Rfi
  AArch64 CoRW1+addrsppal-rfipalp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                ;
   LDR W1,[X0]       ;
   EOR W2,W1,W1      ;
   ADD X3,X0,W2,SXTW ;
   MOV W4,#2         ;
   SUB W5,W4,#1      ;
   STLP W5,W4,[X3]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P DpAddrsW Pa L Rfi
  diyone7: Fatal error: Annotations mismatch between Pa L.
  [2]

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

# Mixed and fullmixed annotations

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW A.b0 Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsw0a.b0-rfia.b0w0 [DpCtrlsWw0A.b0 RfiA.b0w0] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.b0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsa.b0w0-rfiw0a.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0            ;
   LDARB W1,[X0] ;
   CBNZ W1,LC00  ;
   LC00:         ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 A DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsa.b0w0-rfiw0a.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0            ;
   LDARB W1,[X0] ;
   CBNZ W1,LC00  ;
   LC00:         ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW A.h0 Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsw0a.h0-rfia.h0w0 [DpCtrlsWw0A.h0 RfiA.h0w0] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.h0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsa.h0w0-rfiw0a.h0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0            ;
   LDARH W1,[X0] ;
   CBNZ W1,LC00  ;
   LC00:         ;
   STR W2,[X0]   ;
  
  exists (0:X1=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW A.w0 Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsw0a.w0-rfia.w0w0 [DpCtrlsWw0A.w0 RfiA.w0w0] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.w0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsa.w0w0-rfiw0a.w0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDAR W1,[X0] ;
   CBNZ W1,LC00 ;
   LC00:        ;
   STR W2,[X0]  ;
  
  exists (0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW L.b0 Rfi
  AArch64 CoRW1+ctrlsw0l.b0-rfil.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   CBNZ W0,LC00  ;
   LC00:         ;
   MOV W2,#1     ;
   STLRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW L b0 Rfi
  AArch64 CoRW1+ctrlsw0l.b0-rfil.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   CBNZ W0,LC00  ;
   LC00:         ;
   MOV W2,#1     ;
   STLRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.b0 DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsl.b0w0-rfiw0l.b0 [DpCtrlsWL.b0w0 Rfiw0L.b0] failed:
  annotation mismatch on edge DpCtrlsWL.b0w0, annotation 'L.b0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW L.h0 Rfi
  AArch64 CoRW1+ctrlsw0l.h0-rfil.h0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   CBNZ W0,LC00  ;
   LC00:         ;
   MOV W2,#257   ;
   STLRH W2,[X1] ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.h0 DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsl.h0w0-rfiw0l.h0 [DpCtrlsWL.h0w0 Rfiw0L.h0] failed:
  annotation mismatch on edge DpCtrlsWL.h0w0, annotation 'L.h0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW L.w0 Rfi
  AArch64 CoRW1+ctrlsw0l.w0-rfil.w0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   CBNZ W0,LC00 ;
   LC00:        ;
   STLR W2,[X1] ;
  
  exists (0:X0=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.w0 DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsl.w0w0-rfiw0l.w0 [DpCtrlsWL.w0w0 Rfiw0L.w0] failed:
  annotation mismatch on edge DpCtrlsWL.w0w0, annotation 'L.w0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW X.b0 Rfi
  AArch64 CoRW1+ctrlsw0x.b0-rfix.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x;
  }
   P0               ;
   LDR W0,[X1]      ;
   CBNZ W0,LC00     ;
   LC00:            ;
   MOV W2,#1        ;
   Loop01:          ;
   LDXRB W3,[X1]    ;
   STXRB W4,W2,[X1] ;
   CBNZ W4,Loop01   ;
  
  exists ([x]=1 /\ 0:X0=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.b0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsx.b0w0-rfiw0x.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXRB W1,[X0]    ;
   STXRB W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   CBNZ W1,LC01     ;
   LC01:            ;
   STR W3,[X0]      ;
  
  exists ([x]=16843009 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW X.h0 Rfi
  AArch64 CoRW1+ctrlsw0x.h0-rfix.h0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x;
  }
   P0               ;
   LDR W0,[X1]      ;
   CBNZ W0,LC00     ;
   LC00:            ;
   MOV W2,#257      ;
   Loop01:          ;
   LDXRH W3,[X1]    ;
   STXRH W4,W2,[X1] ;
   CBNZ W4,Loop01   ;
  
  exists ([x]=257 /\ 0:X0=257 /\ 0:X3=256)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.h0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsx.h0w0-rfiw0x.h0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXRH W1,[X0]    ;
   STXRH W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   CBNZ W1,LC01     ;
   LC01:            ;
   STR W3,[X0]      ;
  
  exists ([x]=16843009 /\ 0:X1=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW X.w0 Rfi
  AArch64 CoRW1+ctrlsw0x.w0-rfix.w0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; 0:X2=16843009;
  }
   P0              ;
   LDR W0,[X1]     ;
   CBNZ W0,LC00    ;
   LC00:           ;
   Loop01:         ;
   LDXR W3,[X1]    ;
   STXR W4,W2,[X1] ;
   CBNZ W4,Loop01  ;
  
  exists ([x]=16843009 /\ 0:X0=16843009 /\ 0:X3=16843008)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.w0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsx.w0w0-rfiw0x.w0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   CBNZ W1,LC01    ;
   LC01:           ;
   STR W3,[X0]     ;
  
  exists ([x]=16843009 /\ 0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW b0 Rfi
  AArch64 CoRW1+ctrlsw0b0-rfib0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0           ;
   LDR W0,[X1]  ;
   CBNZ W0,LC00 ;
   LC00:        ;
   MOV W2,#1    ;
   STRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsb0w0-rfiw0b0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDRB W0,[X1] ;
   CBNZ W0,LC00 ;
   LC00:        ;
   STR W2,[X1]  ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW b1 Rfi
  AArch64 CoRW1+ctrlsw0b1-rfib1w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0              ;
   LDR W0,[X1]     ;
   CBNZ W0,LC00    ;
   LC00:           ;
   MOV W2,#1       ;
   STRB W2,[X1,#1] ;
  
  exists (0:X0=256)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b1 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsb1w0-rfiw0b1
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0              ;
   LDRB W0,[X1,#1] ;
   CBNZ W0,LC00    ;
   LC00:           ;
   STR W2,[X1]     ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P DpCtrlsW h0 Rfi
  AArch64 CoRW1+ctrlsw0h0-rfih0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0           ;
   LDR W0,[X1]  ;
   CBNZ W0,LC00 ;
   LC00:        ;
   MOV W2,#257  ;
   STRH W2,[X1] ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc h0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsh0w0-rfiw0h0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDRH W0,[X1] ;
   CBNZ W0,LC00 ;
   LC00:        ;
   STR W2,[X1]  ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P DpCtrlsW h2 Rfi
  AArch64 CoRW1+ctrlsw0h2-rfih2w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0              ;
   LDR W0,[X1]     ;
   CBNZ W0,LC00    ;
   LC00:           ;
   MOV W2,#257     ;
   STRH W2,[X1,#2] ;
  
  exists (0:X0=16842752)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc h2 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsh2w0-rfiw0h2
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0              ;
   LDRH W0,[X1,#2] ;
   CBNZ W0,LC00    ;
   LC00:           ;
   STR W2,[X1]     ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P DpCtrlsW w0 Rfi
  AArch64 CoRW1+ctrlsw0w0-rfiw0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   CBNZ W0,LC00 ;
   LC00:        ;
   STR W2,[X1]  ;
  
  exists (0:X0=16843009)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc w0 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsw0w0-rfiw0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   CBNZ W0,LC00 ;
   LC00:        ;
   STR W2,[X1]  ;
  
  exists (0:X0=16843009)

# SIMD annotations

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW NeP Rfi
  AArch64 CoRW1+ctrlspnep-rfinepp
  Variant=neon
  {
   0:X0=x;
  }
   P0            ;
   LDR W1,[X0]   ;
   CBNZ W1,LC00  ;
   LC00:         ;
   MOVI V0.4S,#1 ;
   STUR S0,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsnepp-rfipnep
  Variant=neon
  {
   0:X0=x;
  }
   P0           ;
   LDUR S0,[X0] ;
   FMOV W1,S0   ;
   CBNZ W1,LC00 ;
   LC00:        ;
   MOV W2,#1    ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW NeQ Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspneq-rfineqp [DpCtrlsWPNeQ RfiNeQP] failed:
  No store acquirePc
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeQ DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsneqp-rfipneq
  Variant=neon
  {
   0:X0=x;
  }
   P0             ;
   LDAPUR S0,[X0] ;
   FMOV W1,S0     ;
   CBNZ W1,LC00   ;
   LC00:          ;
   MOV W2,#1      ;
   STR W2,[X0]    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP Q DpCtrlsW P Rfi
  diyone7: Fatal error: Invalid extra annotation NeP
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW NeL Rfi
  AArch64 CoRW1+ctrlspnel-rfinelp
  Variant=neon
  {
   0:X0=x;
  }
   P0            ;
   LDR W1,[X0]   ;
   CBNZ W1,LC00  ;
   LC00:         ;
   MOVI V0.4S,#1 ;
   STLUR S0,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW L NeP Rfi
  diyone7: Fatal error: Annotations mismatch between L NeP.
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeL DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsnelp-rfipnel [DpCtrlsWNeLP RfiPNeL] failed:
  annotation mismatch on edge DpCtrlsWNeLP, annotation 'NeL' on R
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW NePa Rfi
  AArch64 CoRW1+ctrlspnepa-rfinepap
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDR W1,[X0]    ;
   CBNZ W1,LC00   ;
   LC00:          ;
   MOVI V0.4S,#1  ;
   MOVI V1.4S,#2  ;
   STP S0,S1,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePa DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsnepap-rfipnepa
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDP S0,S1,[X0]        ;
   ADD V2.4S,V0.4S,V1.4S ;
   FMOV W1,S2            ;
   CBNZ W1,LC00          ;
   LC00:                 ;
   MOV W2,#1             ;
   STR W2,[X0]           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW NePaN Rfi
  AArch64 CoRW1+ctrlspnepan-rfinepanp
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   CBNZ W1,LC00    ;
   LC00:           ;
   MOVI V0.4S,#1   ;
   MOVI V1.4S,#2   ;
   STNP S0,S1,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePaN DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsnepanp-rfipnepan
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDNP S0,S1,[X0]       ;
   ADD V2.4S,V0.4S,V1.4S ;
   FMOV W1,S2            ;
   CBNZ W1,LC00          ;
   LC00:                 ;
   MOV W2,#1             ;
   STR W2,[X0]           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne1 Rfi
  AArch64 CoRW1+ctrlspne1-rfine1p
  Variant=neon
  {
   int x[4]={0,0,0,0};
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOVI V0.4S,#1    ;
   ST1 {V0.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne1 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne1p-rfipne1
  Variant=neon
  {
   int x[4]={0,0,0,0};
   0:X0=x;
  }
   P0               ;
   LD1 {V0.4S},[X0] ;
   ADDV S1,V0.4S    ;
   FMOV W1,S1       ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#1        ;
   STR W2,[X0]      ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne2 Rfi
  AArch64 CoRW1+ctrlspne2-rfine2p
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LDR W1,[X0]             ;
   CBNZ W1,LC00            ;
   LC00:                   ;
   MOVI V0.4S,#1           ;
   MOVI V1.4S,#2           ;
   ST1 {V0.4S, V1.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne2p-rfipne2
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
   CBNZ W1,LC00            ;
   LC00:                   ;
   MOV W2,#1               ;
   STR W2,[X0]             ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne3 Rfi
  AArch64 CoRW1+ctrlspne3-rfine3p
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LDR W1,[X0]                    ;
   CBNZ W1,LC00                   ;
   LC00:                          ;
   MOVI V0.4S,#1                  ;
   MOVI V1.4S,#2                  ;
   MOVI V2.4S,#3                  ;
   ST1 {V0.4S, V1.4S, V2.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne3p-rfipne3
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
   CBNZ W1,LC00                   ;
   LC00:                          ;
   MOV W2,#1                      ;
   STR W2,[X0]                    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne4 Rfi
  AArch64 CoRW1+ctrlspne4-rfine4p
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LDR W1,[X0]                           ;
   CBNZ W1,LC00                          ;
   LC00:                                 ;
   MOVI V0.4S,#1                         ;
   MOVI V1.4S,#2                         ;
   MOVI V2.4S,#3                         ;
   MOVI V3.4S,#4                         ;
   ST1 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4 DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne4p-rfipne4
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
   CBNZ W1,LC00                          ;
   LC00:                                 ;
   MOV W2,#1                             ;
   STR W2,[X0]                           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne2i Rfi
  AArch64 CoRW1+ctrlspne2i-rfine2ip
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LDR W1,[X0]             ;
   CBNZ W1,LC00            ;
   LC00:                   ;
   MOVI V0.4S,#1           ;
   MOVI V1.4S,#2           ;
   ST2 {V0.4S, V1.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2i DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne2ip-rfipne2i
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
   CBNZ W1,LC00            ;
   LC00:                   ;
   MOV W2,#1               ;
   STR W2,[X0]             ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne3i Rfi
  AArch64 CoRW1+ctrlspne3i-rfine3ip
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LDR W1,[X0]                    ;
   CBNZ W1,LC00                   ;
   LC00:                          ;
   MOVI V0.4S,#1                  ;
   MOVI V1.4S,#2                  ;
   MOVI V2.4S,#3                  ;
   ST3 {V0.4S, V1.4S, V2.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3i DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne3ip-rfipne3i
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
   CBNZ W1,LC00                   ;
   LC00:                          ;
   MOV W2,#1                      ;
   STR W2,[X0]                    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P DpCtrlsW Ne4i Rfi
  AArch64 CoRW1+ctrlspne4i-rfine4ip
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LDR W1,[X0]                           ;
   CBNZ W1,LC00                          ;
   LC00:                                 ;
   MOVI V0.4S,#1                         ;
   MOVI V1.4S,#2                         ;
   MOVI V2.4S,#3                         ;
   MOVI V3.4S,#4                         ;
   ST4 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4i DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsne4ip-rfipne4i
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
   CBNZ W1,LC00                          ;
   LC00:                                 ;
   MOV W2,#1                             ;
   STR W2,[X0]                           ;
  
  exists (0:X1=1)

# Morello annotations

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpCtrlsW Pc Rfi
  AArch64 CoRW1+ctrlsppc-rfipcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1; __uint128 0:X2=0;
  }
   P0           ;
   LDR X1,[C0]  ;
   CBNZ X1,LC00 ;
   LC00:        ;
   MOV X2,#1    ;
   STR C2,[C0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspcp-rfippc
  Variant=morello
  {
   __uint128 x=0;
   __uint128 0:X0=0; 0:X1=0xffffc0000:x:1;
  }
   P0            ;
   LDR C0,[C1]   ;
   GCVALUE X0,C0 ;
   CBNZ X0,LC00  ;
   LC00:         ;
   MOV X2,#1     ;
   STR X2,[C1]   ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpCtrlsW Ac Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspac-rfiacp [DpCtrlsWPAc RfiAcP] failed:
  No store acquire
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ac DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsacp-rfipac
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0            ;
   LDAR C1,[C0]  ;
   GCVALUE X1,C1 ;
   CBNZ X1,LC00  ;
   LC00:         ;
   MOV X2,#1     ;
   STR X2,[C0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc A DpCtrlsW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pc
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpCtrlsW Lc Rfi
  AArch64 CoRW1+ctrlsplc-rfilcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1; __uint128 0:X2=0;
  }
   P0           ;
   LDR X1,[C0]  ;
   CBNZ X1,LC00 ;
   LC00:        ;
   MOV X2,#1    ;
   STLR C2,[C0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpCtrlsW L Pc Rfi
  diyone7: Fatal error: Annotations mismatch between L Pc.
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Lc DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlslcp-rfiplc [DpCtrlsWLcP RfiPLc] failed:
  annotation mismatch on edge DpCtrlsWLcP, annotation 'Lc' on R
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpCtrlsW Ct Rfi
  AArch64 CoRW1+ctrlspct-rfictp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0           ;
   LDR X1,[C0]  ;
   CBNZ X1,LC00 ;
   LC00:        ;
   MOV X2,#1    ;
   STCT X2,[X0] ;
  
  exists (0:X1=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ct DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsctp-rfipct
  Variant=morello
  {
   __uint128 x=0;
   0:X1=0xffffc0000:x:1;
  }
   P0           ;
   LDCT X0,[X1] ;
   CBNZ X0,LC00 ;
   LC00:        ;
   MOV X2,#1    ;
   STR X2,[C1]  ;
  
  exists (0:X0=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P DpCtrlsW Cs Rfi
  AArch64 CoRW1+ctrlspcs-rficsp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0               ;
   LDR X1,[C0]      ;
   CBNZ X1,LC00     ;
   LC00:            ;
   MOV X2,#0        ;
   MOV X3,#1        ;
   SCVALUE C2,C0,X2 ;
   SEAL C2,C2,C3    ;
   STR C2,[C0]      ;
  
  exists (0:X1=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Cs DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlscsp-rfipcs
  Variant=morello
  {
   __uint128 x=0;
   __uint128 0:X0=0; 0:X1=0xffffc0000:x:1;
  }
   P0           ;
   LDR C0,[C1]  ;
   GCTYPE X0,C0 ;
   CBNZ X0,LC00 ;
   LC00:        ;
   MOV X2,#1    ;
   STR X2,[C1]  ;
  
  exists (0:X0=0)

# Ifetch annotations

  $ diyone7 -arch AArch64 -variant ifetch -metadata false PodWRPI FreIP PodWR Fre
  AArch64 SB+po+popi
  Variant=ifetch
  {
   0:X1=x;
   1:X0=instr:"NOP"; 1:X1=x; 1:X2=P0:Lself00;
  }
   P0              | P1          ;
   MOV W0,#1       | STR W0,[X2] ;
   STR W0,[X1]     | LDR W3,[X1] ;
   Lself00: B .+12 |             ;
   MOV W2,#2       |             ;
   B .+8           |             ;
   MOV W2,#1       |             ;
  
  exists (0:X2=1 /\ 1:X3=0)


  $ diyone7 -arch AArch64 -variant ifetch -metadata false PodRW RfePI PodRWIP Rfe
  AArch64 LB+po+poip
  Variant=ifetch
  {
   0:X0=x; 0:X2=instr:"NOP"; 0:X3=P1:Lself00;
   1:X0=x;
  }
   P0          | P1              ;
   LDR W1,[X0] | Lself00: B .+12 ;
   STR W2,[X3] | MOV W1,#2       ;
               | B .+8           ;
               | MOV W1,#1       ;
               | MOV W4,#1       ;
               | STR W4,[X0]     ;
  
  exists (0:X1=1 /\ 1:X1=2)


  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc P DpCtrlsW I Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc I DpCtrlsW P Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

# MemTag annotations

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc P DpCtrlsW T Rfi
  AArch64 CoRW1+ctrlspt-rfitp
  Variant=memtag
  {
   0:X0=x:red; 0:X2=x:green;
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STG X0,[X2]      ;
  
  exists (0:X1=0 /\ not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc T DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlstp-rfipt
  Variant=memtag
  {
   0:X1=x:green;
  }
   P0           ;
   MOV X0,X1    ;
   LDG X0,[X1]  ;
   CBNZ X0,LC00 ;
   LC00:        ;
   MOV W2,#1    ;
   STR W2,[X1]  ;
  
  exists (0:X0=x:green)

# VMSA annotations

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW Pte Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsppte-rfiptep [DpCtrlsWPPte RfiPteP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsptep-rfippte
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDR X1,[X0]  ;
   CBNZ X1,LC00 ;
   LC00:        ;
   MOV W2,#2    ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW Pte A Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspptea-rfipteap [DpCtrlsWPPteA RfiPteAP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte A DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspteap-rfipptea
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   CBNZ X1,LC00 ;
   LC00:        ;
   MOV W2,#2    ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW Pte Q Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsppteq-rfipteqp [DpCtrlsWPPteQ RfiPteQP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte Q DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspteqp-rfippteq
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0            ;
   LDAPR X1,[X0] ;
   CBNZ X1,LC00  ;
   LC00:         ;
   MOV W2,#2     ;
   STR W2,[X3]   ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteOA Rfi
  AArch64 CoRW1+ctrlsppteoa-rfipteoap
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0           ;
   LDR W1,[X0]  ;
   CBNZ W1,LC00 ;
   LC00:        ;
   STR X3,[X2]  ;
  
  exists (true)

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteOA DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspteoap-rfippteoa [DpCtrlsWPteOAP RfiPPteOA] failed:
  annotation mismatch on edge DpCtrlsWPteOAP, annotation 'PteOA' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteV1 Rfi
  AArch64 CoRW1+ctrlspptev1-rfiptev1p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteV1 DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsptev1p-rfipptev1 [DpCtrlsWPteV1P RfiPPteV1] failed:
  annotation mismatch on edge DpCtrlsWPteV1P, annotation 'PteV1' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteAF0 Rfi
  AArch64 CoRW1+ctrlsppteaf0-rfipteaf0p
  Variant=vmsa
  {
   [x]=1;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteAF0 DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlspteaf0p-rfippteaf0 [DpCtrlsWPteAF0P RfiPPteAF0] failed:
  annotation mismatch on edge DpCtrlsWPteAF0P, annotation 'PteAF0' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteHA Rfi
  AArch64 CoRW1+ctrlsppteha-rfiptehap
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHA DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsptehap-rfippteha
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   CBNZ X1,LC00     ;
   LC00:            ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteHD Rfi
  AArch64 CoRW1+ctrlspptehd-rfiptehdp
  Variant=vmsa
  TTHM=HD
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), db:0, dbm:1);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHD DpCtrlsW P Rfi
  diyone7: Fatal error: Test CoRW1+ctrlsptehdp-rfipptehd [DpCtrlsWPteHDP RfiPPteHD] failed:
  annotation mismatch on edge DpCtrlsWPteHDP, annotation 'PteHD' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteV1 PteAF0 Rfi
  AArch64 CoRW1+ctrlspptev1.af0-rfiptev1.af0p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteV1 PteOA Rfi
  AArch64 CoRW1+ctrlsppteoa.v1-rfipteoa.v1p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteAF0 PteOA Rfi
  AArch64 CoRW1+ctrlsppteoa.af0-rfipteoa.af0p
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteV1 PteAF0 PteOA Rfi
  AArch64 CoRW1+ctrlsppteoa.v1.af0-rfipteoa.v1.af0p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW PteHA PteHD Rfi
  AArch64 CoRW1+ctrlsppteha.hd-rfipteha.hdp
  Variant=vmsa
  TTHM=HA HD
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0, db:0, dbm:1);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ fault(P0:L00,x) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A Pte DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlspteap-rfipptea
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   CBNZ X1,LC00 ;
   LC00:        ;
   MOV W2,#2    ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A PteHA DpCtrlsW P Rfi
  AArch64 CoRW1+ctrlsptehaap-rfipptehaa
  Variant=vmsa
  TTHM=HA
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0);
   0:X0=x;
  }
   P0                ;
   L01: LDAR W1,[X0] ;
   CBNZ X1,LC00      ;
   LC00:             ;
   MOV W2,#2         ;
   L00: STR W2,[X0]  ;
  
  exists (0:X1=2 /\ not (fault(P0:L00,x)) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW L PteOA Rfi
  AArch64 CoRW1+ctrlsppteoal-rfipteoalp
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0           ;
   LDR W1,[X0]  ;
   CBNZ W1,LC00 ;
   LC00:        ;
   STLR X3,[X2] ;
  
  exists (true)

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P DpCtrlsW L PteV1 PteAF0 Rfi
  AArch64 CoRW1+ctrlspptev1.af0l-rfiptev1.af0lp
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   CBNZ W1,LC00     ;
   LC00:            ;
   STLR X3,[X2]     ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte X DpCtrlsW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pte
  [2]
