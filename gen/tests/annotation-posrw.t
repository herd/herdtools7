AArch64 annotation compilation checks

# Baseline annotations

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW P Rfi
  AArch64 CoRW1+pos-rfi
  {
   0:X0=x;
  }
   P0          ;
   LDR W1,[X0] ;
   MOV W2,#1   ;
   STR W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW A Rfi
  diyone7: Fatal error: Test CoRW1+pospa-rfiap [PosRWPA RfiAP] failed:
  annotation mismatch on edge RfiAP, annotation 'A' on W
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc A PosRW P Rfi
  AArch64 CoRW1+posap-rfipa
  {
   0:X0=x;
  }
   P0           ;
   LDAR W1,[X0] ;
   MOV W2,#1    ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW Q Rfi
  diyone7: Fatal error: Test CoRW1+pospq-rfiqp [PosRWPQ RfiQP] failed:
  annotation mismatch on edge RfiQP, annotation 'Q' on W
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc Q PosRW P Rfi
  AArch64 CoRW1+posqp-rfipq
  {
   0:X0=x;
  }
   P0            ;
   LDAPR W1,[X0] ;
   MOV W2,#1     ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW L Rfi
  AArch64 CoRW1+pospl-rfilp
  {
   0:X0=x;
  }
   P0           ;
   LDR W1,[X0]  ;
   MOV W2,#1    ;
   STLR W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc L PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+poslp-rfipl [PosRWLP RfiPL] failed:
  annotation mismatch on edge PosRWLP, annotation 'L' on R
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW X Rfi
  AArch64 CoRW1+pospx-rfixp
  {
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   MOV W2,#1       ;
   Loop00:         ;
   LDXR W3,[X0]    ;
   STXR W4,W2,[X0] ;
   CBNZ W4,Loop00  ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc X PosRW P Rfi
  AArch64 CoRW1+posxp-rfipx
  {
   0:X0=x;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW XA Rfi
  AArch64 CoRW1+pospxa-rfixap
  {
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   MOV W2,#1       ;
   Loop00:         ;
   LDAXR W3,[X0]   ;
   STXR W4,W2,[X0] ;
   CBNZ W4,Loop00  ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XA PosRW P Rfi
  AArch64 CoRW1+posxap-rfipxa
  {
   0:X0=x;
  }
   P0              ;
   Loop00:         ;
   LDAXR W1,[X0]   ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW XL Rfi
  AArch64 CoRW1+pospxl-rfixlp
  {
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   MOV W2,#1        ;
   Loop00:          ;
   LDXR W3,[X0]     ;
   STLXR W4,W2,[X0] ;
   CBNZ W4,Loop00   ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XL PosRW P Rfi
  AArch64 CoRW1+posxlp-rfipxl
  {
   0:X0=x;
  }
   P0               ;
   Loop00:          ;
   LDXR W1,[X0]     ;
   STLXR W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   MOV W3,#1        ;
   STR W3,[X0]      ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW XAL Rfi
  AArch64 CoRW1+pospxal-rfixalp
  {
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   MOV W2,#1        ;
   Loop00:          ;
   LDAXR W3,[X0]    ;
   STLXR W4,W2,[X0] ;
   CBNZ W4,Loop00   ;
  
  exists ([x]=1 /\ 0:X1=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -metadata false -oneloc XAL PosRW P Rfi
  AArch64 CoRW1+posxalp-rfipxal
  {
   0:X0=x;
  }
   P0               ;
   Loop00:          ;
   LDAXR W1,[X0]    ;
   STLXR W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   MOV W3,#1        ;
   STR W3,[X0]      ;
  
  exists ([x]=1 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW Pa Rfi
  AArch64 CoRW1+posppa-rfipap
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDR W1,[X0]    ;
   MOV W2,#2      ;
   SUB W3,W2,#1   ;
   STP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa PosRW P Rfi
  AArch64 CoRW1+pospap-rfippa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDP W1,W2,[X0] ;
   ADD W1,W1,W2   ;
   MOV W3,#1      ;
   STR W3,[X0]    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW PaN Rfi
  AArch64 CoRW1+posppan-rfipanp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   MOV W2,#2       ;
   SUB W3,W2,#1    ;
   STNP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc PaN PosRW P Rfi
  AArch64 CoRW1+pospanp-rfippan
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDNP W1,W2,[X0] ;
   ADD W1,W1,W2    ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW PaIL Rfi
  AArch64 CoRW1+posppail-rfipailp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   MOV W2,#2        ;
   SUB W3,W2,#1     ;
   STILP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc PaIQ PosRW P Rfi
  AArch64 CoRW1+pospaiqp-rfippaiq
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                ;
   LDIAPP W1,W2,[X0] ;
   ADD W1,W1,W2      ;
   MOV W3,#1         ;
   STR W3,[X0]       ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW PaL Rfi
  AArch64 CoRW1+posppal-rfipalp
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   MOV W2,#2       ;
   SUB W3,W2,#1    ;
   STLP W3,W2,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P PosRW Pa L Rfi
  diyone7: Fatal error: Annotations mismatch between Pa L.
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaA PosRW P Rfi
  AArch64 CoRW1+pospaap-rfippaa
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDAP W1,W2,[X0] ;
   ADD W1,W1,W2    ;
   MOV W3,#1       ;
   STR W3,[X0]     ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa A PosRW P Rfi
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

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW A.b0 Rfi
  diyone7: Fatal error: Test CoRW1+posw0a.b0-rfia.b0w0 [PosRWw0A.b0 RfiA.b0w0] failed:
  annotation mismatch on edge RfiA.b0w0, annotation 'A.b0' on W
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.b0 PosRW P Rfi
  AArch64 CoRW1+posa.b0w0-rfiw0a.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0            ;
   LDARB W1,[X0] ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 A PosRW P Rfi
  AArch64 CoRW1+posa.b0w0-rfiw0a.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0            ;
   LDARB W1,[X0] ;
   STR W2,[X0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW A.h0 Rfi
  diyone7: Fatal error: Test CoRW1+posw0a.h0-rfia.h0w0 [PosRWw0A.h0 RfiA.h0w0] failed:
  annotation mismatch on edge RfiA.h0w0, annotation 'A.h0' on W
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.h0 PosRW P Rfi
  AArch64 CoRW1+posa.h0w0-rfiw0a.h0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0            ;
   LDARH W1,[X0] ;
   STR W2,[X0]   ;
  
  exists (0:X1=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW A.w0 Rfi
  diyone7: Fatal error: Test CoRW1+posw0a.w0-rfia.w0w0 [PosRWw0A.w0 RfiA.w0w0] failed:
  annotation mismatch on edge RfiA.w0w0, annotation 'A.w0' on W
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.w0 PosRW P Rfi
  AArch64 CoRW1+posa.w0w0-rfiw0a.w0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDAR W1,[X0] ;
   STR W2,[X0]  ;
  
  exists (0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW L.b0 Rfi
  AArch64 CoRW1+posw0l.b0-rfil.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   MOV W2,#1     ;
   STLRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW L b0 Rfi
  AArch64 CoRW1+posw0l.b0-rfil.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   MOV W2,#1     ;
   STLRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.b0 PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+posl.b0w0-rfiw0l.b0 [PosRWL.b0w0 Rfiw0L.b0] failed:
  annotation mismatch on edge PosRWL.b0w0, annotation 'L.b0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW L.h0 Rfi
  AArch64 CoRW1+posw0l.h0-rfil.h0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0            ;
   LDR W0,[X1]   ;
   MOV W2,#257   ;
   STLRH W2,[X1] ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.h0 PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+posl.h0w0-rfiw0l.h0 [PosRWL.h0w0 Rfiw0L.h0] failed:
  annotation mismatch on edge PosRWL.h0w0, annotation 'L.h0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW L.w0 Rfi
  AArch64 CoRW1+posw0l.w0-rfil.w0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDR W0,[X1]  ;
   STLR W2,[X1] ;
  
  exists (0:X0=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.w0 PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+posl.w0w0-rfiw0l.w0 [PosRWL.w0w0 Rfiw0L.w0] failed:
  annotation mismatch on edge PosRWL.w0w0, annotation 'L.w0' on R
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW X.b0 Rfi
  AArch64 CoRW1+posw0x.b0-rfix.b0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x;
  }
   P0               ;
   LDR W0,[X1]      ;
   MOV W2,#1        ;
   Loop00:          ;
   LDXRB W3,[X1]    ;
   STXRB W4,W2,[X1] ;
   CBNZ W4,Loop00   ;
  
  exists ([x]=1 /\ 0:X0=1 /\ 0:X3=0)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.b0 PosRW P Rfi
  AArch64 CoRW1+posx.b0w0-rfiw0x.b0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXRB W1,[X0]    ;
   STXRB W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   STR W3,[X0]      ;
  
  exists ([x]=16843009 /\ 0:X1=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW X.h0 Rfi
  AArch64 CoRW1+posw0x.h0-rfix.h0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x;
  }
   P0               ;
   LDR W0,[X1]      ;
   MOV W2,#257      ;
   Loop00:          ;
   LDXRH W3,[X1]    ;
   STXRH W4,W2,[X1] ;
   CBNZ W4,Loop00   ;
  
  exists ([x]=257 /\ 0:X0=257 /\ 0:X3=256)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.h0 PosRW P Rfi
  AArch64 CoRW1+posx.h0w0-rfiw0x.h0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXRH W1,[X0]    ;
   STXRH W2,W1,[X0] ;
   CBNZ W2,Loop00   ;
   STR W3,[X0]      ;
  
  exists ([x]=16843009 /\ 0:X1=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW X.w0 Rfi
  AArch64 CoRW1+posw0x.w0-rfix.w0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; 0:X2=16843009;
  }
   P0              ;
   LDR W0,[X1]     ;
   Loop00:         ;
   LDXR W3,[X1]    ;
   STXR W4,W2,[X1] ;
   CBNZ W4,Loop00  ;
  
  exists ([x]=16843009 /\ 0:X0=16843009 /\ 0:X3=16843008)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.w0 PosRW P Rfi
  AArch64 CoRW1+posx.w0w0-rfiw0x.w0
  Variant=mixed
  {
   0:X0=x; uint32_t 0:X1=0; uint32_t 0:X3=16843009;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W2,W1,[X0] ;
   CBNZ W2,Loop00  ;
   STR W3,[X0]     ;
  
  exists ([x]=16843009 /\ 0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW b0 Rfi
  AArch64 CoRW1+posw0b0-rfib0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0           ;
   LDR W0,[X1]  ;
   MOV W2,#1    ;
   STRB W2,[X1] ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 PosRW P Rfi
  AArch64 CoRW1+posb0w0-rfiw0b0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDRB W0,[X1] ;
   STR W2,[X1]  ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW b1 Rfi
  AArch64 CoRW1+posw0b1-rfib1w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0              ;
   LDR W0,[X1]     ;
   MOV W2,#1       ;
   STRB W2,[X1,#1] ;
  
  exists (0:X0=256)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b1 PosRW P Rfi
  AArch64 CoRW1+posb1w0-rfiw0b1
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0              ;
   LDRB W0,[X1,#1] ;
   STR W2,[X1]     ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P PosRW h0 Rfi
  AArch64 CoRW1+posw0h0-rfih0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0           ;
   LDR W0,[X1]  ;
   MOV W2,#257  ;
   STRH W2,[X1] ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc h0 PosRW P Rfi
  AArch64 CoRW1+posh0w0-rfiw0h0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0           ;
   LDRH W0,[X1] ;
   STR W2,[X1]  ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P PosRW h2 Rfi
  AArch64 CoRW1+posw0h2-rfih2w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=0;
  }
   P0              ;
   LDR W0,[X1]     ;
   MOV W2,#257     ;
   STRH W2,[X1,#2] ;
  
  exists (0:X0=16842752)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc h2 PosRW P Rfi
  AArch64 CoRW1+posh2w0-rfiw0h2
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0              ;
   LDRH W0,[X1,#2] ;
   STR W2,[X1]     ;
  
  exists (0:X0=257)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P PosRW w0 Rfi
  AArch64 CoRW1+posw0w0-rfiw0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0          ;
   LDR W0,[X1] ;
   STR W2,[X1] ;
  
  exists (0:X0=16843009)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc w0 PosRW P Rfi
  AArch64 CoRW1+posw0w0-rfiw0w0
  Variant=mixed
  {
   uint32_t 0:X0=0; 0:X1=x; uint32_t 0:X2=16843009;
  }
   P0          ;
   LDR W0,[X1] ;
   STR W2,[X1] ;
  
  exists (0:X0=16843009)

# SIMD annotations

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW NeP Rfi
  AArch64 CoRW1+pospnep-rfinepp
  Variant=neon
  {
   0:X0=x;
  }
   P0            ;
   LDR W1,[X0]   ;
   MOVI V0.4S,#1 ;
   STUR S0,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP PosRW P Rfi
  AArch64 CoRW1+posnepp-rfipnep
  Variant=neon
  {
   0:X0=x;
  }
   P0           ;
   LDUR S0,[X0] ;
   FMOV W1,S0   ;
   MOV W2,#1    ;
   STR W2,[X0]  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW NeQ Rfi
  diyone7: Fatal error: Test CoRW1+pospneq-rfineqp [PosRWPNeQ RfiNeQP] failed:
  annotation mismatch on edge RfiNeQP, annotation 'NeQ' on W
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeQ PosRW P Rfi
  AArch64 CoRW1+posneqp-rfipneq
  Variant=neon
  {
   0:X0=x;
  }
   P0             ;
   LDAPUR S0,[X0] ;
   FMOV W1,S0     ;
   MOV W2,#1      ;
   STR W2,[X0]    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP Q PosRW P Rfi
  diyone7: Fatal error: Invalid extra annotation NeP
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW NeL Rfi
  AArch64 CoRW1+pospnel-rfinelp
  Variant=neon
  {
   0:X0=x;
  }
   P0            ;
   LDR W1,[X0]   ;
   MOVI V0.4S,#1 ;
   STLUR S0,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW L NeP Rfi
  diyone7: Fatal error: Annotations mismatch between L NeP.
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeL PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+posnelp-rfipnel [PosRWNeLP RfiPNeL] failed:
  annotation mismatch on edge PosRWNeLP, annotation 'NeL' on R
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW NePa Rfi
  AArch64 CoRW1+pospnepa-rfinepap
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0             ;
   LDR W1,[X0]    ;
   MOVI V0.4S,#1  ;
   MOVI V1.4S,#2  ;
   STP S0,S1,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePa PosRW P Rfi
  AArch64 CoRW1+posnepap-rfipnepa
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDP S0,S1,[X0]        ;
   ADD V2.4S,V0.4S,V1.4S ;
   FMOV W1,S2            ;
   MOV W2,#1             ;
   STR W2,[X0]           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW NePaN Rfi
  AArch64 CoRW1+pospnepan-rfinepanp
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0              ;
   LDR W1,[X0]     ;
   MOVI V0.4S,#1   ;
   MOVI V1.4S,#2   ;
   STNP S0,S1,[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePaN PosRW P Rfi
  AArch64 CoRW1+posnepanp-rfipnepan
  Variant=neon
  {
   int x[2]={0,0};
   0:X0=x;
  }
   P0                    ;
   LDNP S0,S1,[X0]       ;
   ADD V2.4S,V0.4S,V1.4S ;
   FMOV W1,S2            ;
   MOV W2,#1             ;
   STR W2,[X0]           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne1 Rfi
  AArch64 CoRW1+pospne1-rfine1p
  Variant=neon
  {
   int x[4]={0,0,0,0};
   0:X0=x;
  }
   P0               ;
   LDR W1,[X0]      ;
   MOVI V0.4S,#1    ;
   ST1 {V0.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne1 PosRW P Rfi
  AArch64 CoRW1+posne1p-rfipne1
  Variant=neon
  {
   int x[4]={0,0,0,0};
   0:X0=x;
  }
   P0               ;
   LD1 {V0.4S},[X0] ;
   ADDV S1,V0.4S    ;
   FMOV W1,S1       ;
   MOV W2,#1        ;
   STR W2,[X0]      ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne2 Rfi
  AArch64 CoRW1+pospne2-rfine2p
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LDR W1,[X0]             ;
   MOVI V0.4S,#1           ;
   MOVI V1.4S,#2           ;
   ST1 {V0.4S, V1.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2 PosRW P Rfi
  AArch64 CoRW1+posne2p-rfipne2
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
   MOV W2,#1               ;
   STR W2,[X0]             ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne3 Rfi
  AArch64 CoRW1+pospne3-rfine3p
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LDR W1,[X0]                    ;
   MOVI V0.4S,#1                  ;
   MOVI V1.4S,#2                  ;
   MOVI V2.4S,#3                  ;
   ST1 {V0.4S, V1.4S, V2.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3 PosRW P Rfi
  AArch64 CoRW1+posne3p-rfipne3
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
   MOV W2,#1                      ;
   STR W2,[X0]                    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne4 Rfi
  AArch64 CoRW1+pospne4-rfine4p
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LDR W1,[X0]                           ;
   MOVI V0.4S,#1                         ;
   MOVI V1.4S,#2                         ;
   MOVI V2.4S,#3                         ;
   MOVI V3.4S,#4                         ;
   ST1 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4 PosRW P Rfi
  AArch64 CoRW1+posne4p-rfipne4
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
   MOV W2,#1                             ;
   STR W2,[X0]                           ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne2i Rfi
  AArch64 CoRW1+pospne2i-rfine2ip
  Variant=neon
  {
   int x[8]={0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                      ;
   LDR W1,[X0]             ;
   MOVI V0.4S,#1           ;
   MOVI V1.4S,#2           ;
   ST2 {V0.4S, V1.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2i PosRW P Rfi
  AArch64 CoRW1+posne2ip-rfipne2i
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
   MOV W2,#1               ;
   STR W2,[X0]             ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne3i Rfi
  AArch64 CoRW1+pospne3i-rfine3ip
  Variant=neon
  {
   int x[12]={0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                             ;
   LDR W1,[X0]                    ;
   MOVI V0.4S,#1                  ;
   MOVI V1.4S,#2                  ;
   MOVI V2.4S,#3                  ;
   ST3 {V0.4S, V1.4S, V2.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3i PosRW P Rfi
  AArch64 CoRW1+posne3ip-rfipne3i
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
   MOV W2,#1                      ;
   STR W2,[X0]                    ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P PosRW Ne4i Rfi
  AArch64 CoRW1+pospne4i-rfine4ip
  Variant=neon
  {
   int x[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   0:X0=x;
  }
   P0                                    ;
   LDR W1,[X0]                           ;
   MOVI V0.4S,#1                         ;
   MOVI V1.4S,#2                         ;
   MOVI V2.4S,#3                         ;
   MOVI V3.4S,#4                         ;
   ST4 {V0.4S, V1.4S, V2.4S, V3.4S},[X0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4i PosRW P Rfi
  AArch64 CoRW1+posne4ip-rfipne4i
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
   MOV W2,#1                             ;
   STR W2,[X0]                           ;
  
  exists (0:X1=1)

# Morello annotations

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P PosRW Pc Rfi
  AArch64 CoRW1+posppc-rfipcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1; __uint128 0:X2=0;
  }
   P0          ;
   LDR X1,[C0] ;
   MOV X2,#1   ;
   STR C2,[C0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc PosRW P Rfi
  AArch64 CoRW1+pospcp-rfippc
  Variant=morello
  {
   __uint128 x=0;
   __uint128 0:X0=0; 0:X1=0xffffc0000:x:1;
  }
   P0            ;
   LDR C0,[C1]   ;
   GCVALUE X0,C0 ;
   MOV X2,#1     ;
   STR X2,[C1]   ;
  
  exists (0:X0=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P PosRW Ac Rfi
  diyone7: Fatal error: Test CoRW1+pospac-rfiacp [PosRWPAc RfiAcP] failed:
  annotation mismatch on edge RfiAcP, annotation 'Ac' on W
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ac PosRW P Rfi
  AArch64 CoRW1+posacp-rfipac
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0            ;
   LDAR C1,[C0]  ;
   GCVALUE X1,C1 ;
   MOV X2,#1     ;
   STR X2,[C0]   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc A PosRW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pc
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P PosRW Lc Rfi
  AArch64 CoRW1+posplc-rfilcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1; __uint128 0:X2=0;
  }
   P0           ;
   LDR X1,[C0]  ;
   MOV X2,#1    ;
   STLR C2,[C0] ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P PosRW L Pc Rfi
  diyone7: Fatal error: Annotations mismatch between L Pc.
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Lc PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+poslcp-rfiplc [PosRWLcP RfiPLc] failed:
  annotation mismatch on edge PosRWLcP, annotation 'Lc' on R
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P PosRW Ct Rfi
  AArch64 CoRW1+pospct-rfictp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0           ;
   LDR X1,[C0]  ;
   MOV X2,#1    ;
   STCT X2,[X0] ;
  
  exists (0:X1=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ct PosRW P Rfi
  AArch64 CoRW1+posctp-rfipct
  Variant=morello
  {
   __uint128 x=0;
   0:X1=0xffffc0000:x:1;
  }
   P0           ;
   LDCT X0,[X1] ;
   MOV X2,#1    ;
   STR X2,[C1]  ;
  
  exists (0:X0=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P PosRW Cs Rfi
  AArch64 CoRW1+pospcs-rficsp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0               ;
   LDR X1,[C0]      ;
   MOV X2,#0        ;
   MOV X3,#1        ;
   SCVALUE C2,C0,X2 ;
   SEAL C2,C2,C3    ;
   STR C2,[C0]      ;
  
  exists (0:X1=0)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Cs PosRW P Rfi
  AArch64 CoRW1+poscsp-rfipcs
  Variant=morello
  {
   __uint128 x=0;
   __uint128 0:X0=0; 0:X1=0xffffc0000:x:1;
  }
   P0           ;
   LDR C0,[C1]  ;
   GCTYPE X0,C0 ;
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


  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc P PosRW I Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc I PosRW P Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

# MemTag annotations

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc P PosRW T Rfi
  AArch64 CoRW1+pospt-rfitp
  Variant=memtag
  {
   0:X0=x:red; 0:X2=x:green;
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STG X0,[X2]      ;
  
  exists (0:X1=0 /\ not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc T PosRW P Rfi
  AArch64 CoRW1+postp-rfipt
  Variant=memtag
  {
   0:X1=x:green;
  }
   P0          ;
   MOV X0,X1   ;
   LDG X0,[X1] ;
   MOV W2,#1   ;
   STR W2,[X1] ;
  
  exists (0:X0=x:green)

# VMSA annotations

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW Pte Rfi
  diyone7: Fatal error: Test CoRW1+posppte-rfiptep [PosRWPPte RfiPteP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte PosRW P Rfi
  AArch64 CoRW1+posptep-rfippte
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0          ;
   LDR X1,[X0] ;
   MOV W2,#2   ;
   STR W2,[X3] ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW Pte A Rfi
  diyone7: Fatal error: Test CoRW1+pospptea-rfipteap [PosRWPPteA RfiPteAP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte A PosRW P Rfi
  AArch64 CoRW1+pospteap-rfipptea
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   MOV W2,#2    ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW Pte Q Rfi
  diyone7: Fatal error: Test CoRW1+posppteq-rfipteqp [PosRWPPteQ RfiPteQP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte Q PosRW P Rfi
  AArch64 CoRW1+pospteqp-rfippteq
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0            ;
   LDAPR X1,[X0] ;
   MOV W2,#2     ;
   STR W2,[X3]   ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteOA Rfi
  AArch64 CoRW1+posppteoa-rfipteoap
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0          ;
   LDR W1,[X0] ;
   STR X3,[X2] ;
  
  exists (true)

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteOA PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+pospteoap-rfippteoa [PosRWPteOAP RfiPPteOA] failed:
  annotation mismatch on edge PosRWPteOAP, annotation 'PteOA' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteV1 Rfi
  AArch64 CoRW1+pospptev1-rfiptev1p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteV1 PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+posptev1p-rfipptev1 [PosRWPteV1P RfiPPteV1] failed:
  annotation mismatch on edge PosRWPteV1P, annotation 'PteV1' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteAF0 Rfi
  AArch64 CoRW1+posppteaf0-rfipteaf0p
  Variant=vmsa
  {
   [x]=1;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteAF0 PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+pospteaf0p-rfippteaf0 [PosRWPteAF0P RfiPPteAF0] failed:
  annotation mismatch on edge PosRWPteAF0P, annotation 'PteAF0' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteHA Rfi
  AArch64 CoRW1+posppteha-rfiptehap
  Variant=vmsa
  TTHM=HA
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

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHA PosRW P Rfi
  AArch64 CoRW1+posptehap-rfippteha
  Variant=vmsa
  TTHM=HA
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

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteHD Rfi
  AArch64 CoRW1+pospptehd-rfiptehdp
  Variant=vmsa
  TTHM=HD
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

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHD PosRW P Rfi
  diyone7: Fatal error: Test CoRW1+posptehdp-rfipptehd [PosRWPteHDP RfiPPteHD] failed:
  annotation mismatch on edge PosRWPteHDP, annotation 'PteHD' on R
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteV1 PteAF0 Rfi
  AArch64 CoRW1+pospptev1.af0-rfiptev1.af0p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteV1 PteOA Rfi
  AArch64 CoRW1+posppteoa.v1-rfipteoa.v1p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (not (fault(P0:L00,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteAF0 PteOA Rfi
  AArch64 CoRW1+posppteoa.af0-rfipteoa.af0p
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteV1 PteAF0 PteOA Rfi
  AArch64 CoRW1+posppteoa.v1.af0-rfipteoa.v1.af0p
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STR X3,[X2]      ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW PteHA PteHD Rfi
  AArch64 CoRW1+posppteha.hd-rfipteha.hdp
  Variant=vmsa
  TTHM=HA HD
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), af:0, db:0, dbm:1);
   0:X0=x;
  }
   P0               ;
   L01: LDR W1,[X0] ;
   MOV W2,#2        ;
   L00: STR W2,[X0] ;
  
  exists (0:X1=2 /\ fault(P0:L00,x) /\ not (fault(P0:L01,x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A Pte PosRW P Rfi
  AArch64 CoRW1+pospteap-rfipptea
  Variant=vmsa
  {
   [x]=1;
   0:X0=PTE(x); pteval_t 0:X1=0; 0:X3=x;
  }
   P0           ;
   LDAR X1,[X0] ;
   MOV W2,#2    ;
   STR W2,[X3]  ;
  
  exists (0:X1=(oa:PA(x)))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A PteHA PosRW P Rfi
  AArch64 CoRW1+posptehaap-rfipptehaa
  Variant=vmsa
  TTHM=HA
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

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW L PteOA Rfi
  AArch64 CoRW1+posppteoal-rfipteoalp
  Variant=vmsa
  {
   [x]=1;
   [y]=5;
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(y));
  }
   P0           ;
   LDR W1,[X0]  ;
   STLR X3,[X2] ;
  
  exists (true)

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P PosRW L PteV1 PteAF0 Rfi
  AArch64 CoRW1+pospptev1.af0l-rfiptev1.af0lp
  Variant=vmsa
  {
   [x]=1;
   [PTE(x)]=(oa:PA(x), valid:0);
   0:X0=x; 0:X2=PTE(x); 0:X3=(oa:PA(x), af:0);
  }
   P0               ;
   L00: LDR W1,[X0] ;
   STLR X3,[X2]     ;
  
  exists (fault(P0:L00,x))

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte X PosRW P Rfi
  diyone7: Fatal error: Invalid extra annotation Pte
  [2]
