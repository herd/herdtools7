AArch64 LxSx annotation compilation checks

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx P Rfi
  AArch64 CoRW1+rmw-rfi
  {
   0:X0=x;
  }
   P0              ;
   MOV W2,#1       ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W3,W2,[X0] ;
   CBNZ W3,Loop00  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx A Rfi
  diyone7: Fatal error: Test CoRW1+rmwpa-rfiap [LxSxPA RfiAP] failed:
  Bad annotation for Sx: A
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc A LxSx P Rfi
  AArch64 CoRW1+rmwap-rfipa
  {
   0:X0=x;
  }
   P0              ;
   MOV W2,#1       ;
   Loop00:         ;
   LDAXR W1,[X0]   ;
   STXR W3,W2,[X0] ;
   CBNZ W3,Loop00  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx Q Rfi
  diyone7: Fatal error: Test CoRW1+rmwpq-rfiqp [LxSxPQ RfiQP] failed:
  Bad annotation for Sx: Q
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc Q LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwqp-rfipq [LxSxQP RfiPQ] failed:
  AcqPC annotation on xload
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx L Rfi
  AArch64 CoRW1+rmwpl-rfilp
  {
   0:X0=x;
  }
   P0               ;
   MOV W2,#1        ;
   Loop00:          ;
   LDXR W1,[X0]     ;
   STLXR W3,W2,[X0] ;
   CBNZ W3,Loop00   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -metadata false -oneloc L LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwlp-rfipl [LxSxLP RfiPL] failed:
  Bad annotation for Lx: L
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx X Rfi
  diyone7: Fatal error: Test CoRW1+rmwpx-rfixp [LxSxPX RfiXP] failed:
  Bad annotation for Sx: X
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc X LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwxp-rfipx [LxSxXP RfiPX] failed:
  Bad annotation for Lx: X
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx XA Rfi
  diyone7: Fatal error: Test CoRW1+rmwpxa-rfixap [LxSxPXA RfiXAP] failed:
  Bad annotation for Sx: XA
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc XA LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwxap-rfipxa [LxSxXAP RfiPXA] failed:
  Bad annotation for Lx: XA
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx XL Rfi
  diyone7: Fatal error: Test CoRW1+rmwpxl-rfixlp [LxSxPXL RfiXLP] failed:
  Bad annotation for Sx: XL
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc XL LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwxlp-rfipxl [LxSxXLP RfiPXL] failed:
  Bad annotation for Lx: XL
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx XAL Rfi
  diyone7: Fatal error: Test CoRW1+rmwpxal-rfixalp [LxSxPXAL RfiXALP] failed:
  Bad annotation for Sx: XAL
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc XAL LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwxalp-rfipxal [LxSxXALP RfiPXAL] failed:
  Bad annotation for Lx: XAL
  
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx Pa Rfi
  diyone7: Fatal error: Test CoRW1+rmwppa-rfipap [LxSxPPa RfiPaP] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpap-rfippa [LxSxPaP RfiPPa] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx PaN Rfi
  diyone7: Fatal error: Test CoRW1+rmwppan-rfipanp [LxSxPPaN RfiPaNP] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaN LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpanp-rfippan [LxSxPaNP RfiPPaN] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx PaIL Rfi
  diyone7: Fatal error: Test CoRW1+rmwppail-rfipailp [LxSxPPaIL RfiPaILP] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaIQ LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpaiqp-rfippaiq [LxSxPaIQP RfiPPaIQ] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx PaL Rfi
  diyone7: Fatal error: Test CoRW1+rmwppal-rfipalp [LxSxPPaL RfiPaLP] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc P LxSx Pa L Rfi
  diyone7: Fatal error: Annotations mismatch between Pa L.
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc PaA LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpaap-rfippaa [LxSxPaAP RfiPPaA] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -metadata false -oneloc Pa A LxSx P Rfi
  diyone7: Fatal error: Invalid extra annotation Pa
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx A.b0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0a.b0-rfia.b0w0 [LxSxw0A.b0 RfiA.b0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.b0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwa.b0w0-rfiw0a.b0 [LxSxA.b0w0 Rfiw0A.b0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 A LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwa.b0w0-rfiw0a.b0 [LxSxA.b0w0 Rfiw0A.b0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx A.h0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0a.h0-rfia.h0w0 [LxSxw0A.h0 RfiA.h0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.h0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwa.h0w0-rfiw0a.h0 [LxSxA.h0w0 Rfiw0A.h0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx A.w0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0a.w0-rfia.w0w0 [LxSxw0A.w0 RfiA.w0w0] failed:
  Bad annotation for Sx: A.w0
  
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc A.w0 LxSx P Rfi
  AArch64 CoRW1+rmwa.w0w0-rfiw0a.w0
  Variant=mixed
  {
   0:X0=x; 0:X2=16843009;
  }
   P0              ;
   Loop00:         ;
   LDAXR W1,[X0]   ;
   STXR W3,W2,[X0] ;
   CBNZ W3,Loop00  ;
  
  exists (0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx L.b0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0l.b0-rfil.b0w0 [LxSxw0L.b0 RfiL.b0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx L b0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0l.b0-rfil.b0w0 [LxSxw0L.b0 RfiL.b0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.b0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwl.b0w0-rfiw0l.b0 [LxSxL.b0w0 Rfiw0L.b0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx L.h0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0l.h0-rfil.h0w0 [LxSxw0L.h0 RfiL.h0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.h0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwl.h0w0-rfiw0l.h0 [LxSxL.h0w0 Rfiw0L.h0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx L.w0 Rfi
  AArch64 CoRW1+rmww0l.w0-rfil.w0w0
  Variant=mixed
  {
   0:X0=x; 0:X2=16843009;
  }
   P0               ;
   Loop00:          ;
   LDXR W1,[X0]     ;
   STLXR W3,W2,[X0] ;
   CBNZ W3,Loop00   ;
  
  exists (0:X1=16843009)

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc L.w0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwl.w0w0-rfiw0l.w0 [LxSxL.w0w0 Rfiw0L.w0] failed:
  Bad annotation for Lx: L.w0
  
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx X.b0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0x.b0-rfix.b0w0 [LxSxw0X.b0 RfiX.b0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.b0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwx.b0w0-rfiw0x.b0 [LxSxX.b0w0 Rfiw0X.b0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx X.h0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0x.h0-rfix.h0w0 [LxSxw0X.h0 RfiX.h0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.h0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwx.h0w0-rfiw0x.h0 [LxSxX.h0w0 Rfiw0X.h0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx X.w0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0x.w0-rfix.w0w0 [LxSxw0X.w0 RfiX.w0w0] failed:
  Bad annotation for Sx: X.w0
  
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc X.w0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwx.w0w0-rfiw0x.w0 [LxSxX.w0w0 Rfiw0X.w0] failed:
  Bad annotation for Lx: X.w0
  
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx b0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0b0-rfib0w0 [LxSxw0b0 Rfib0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwb0w0-rfiw0b0 [LxSxb0w0 Rfiw0b0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx b1 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0b1-rfib1w0 [LxSxw0b1 Rfib1w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc b1 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwb1w0-rfiw0b1 [LxSxb1w0 Rfiw0b1] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P LxSx h0 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0h0-rfih0w0 [LxSxw0h0 Rfih0w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc h0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwh0w0-rfiw0h0 [LxSxh0w0 Rfiw0h0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc P LxSx h2 Rfi
  diyone7: Fatal error: Test CoRW1+rmww0h2-rfih2w0 [LxSxw0h2 Rfih2w0] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant fullmixed -metadata false -oneloc h2 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwh2w0-rfiw0h2 [LxSxh2w0 Rfiw0h2] failed:
  Refuse to generate constrained unpredictable, use -variant CU to accept
  [2]

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc P LxSx w0 Rfi
  AArch64 CoRW1+rmww0w0-rfiw0w0
  Variant=mixed
  {
   0:X0=x; 0:X2=16843009;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W3,W2,[X0] ;
   CBNZ W3,Loop00  ;
  
  exists (0:X1=16843009)

  $ diyone7 -arch AArch64 -variant mixed -metadata false -oneloc w0 LxSx P Rfi
  AArch64 CoRW1+rmww0w0-rfiw0w0
  Variant=mixed
  {
   0:X0=x; 0:X2=16843009;
  }
   P0              ;
   Loop00:         ;
   LDXR W1,[X0]    ;
   STXR W3,W2,[X0] ;
   CBNZ W3,Loop00  ;
  
  exists (0:X1=16843009)

# SIMD annotations

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx NeP Rfi
  diyone7: Fatal error: Test CoRW1+rmwpnep-rfinepp [LxSxPNeP RfiNePP] failed:
  Bad annotation for Sx: NeP
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwnepp-rfipnep [LxSxNePP RfiPNeP] failed:
  Bad annotation for Lx: NeP
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx NeQ Rfi
  diyone7: Fatal error: Test CoRW1+rmwpneq-rfineqp [LxSxPNeQ RfiNeQP] failed:
  Bad annotation for Sx: NeQ
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeQ LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwneqp-rfipneq [LxSxNeQP RfiPNeQ] failed:
  AcqPC annotation on xload
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeP Q LxSx P Rfi
  diyone7: Fatal error: Invalid extra annotation NeP
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx NeL Rfi
  diyone7: Fatal error: Test CoRW1+rmwpnel-rfinelp [LxSxPNeL RfiNeLP] failed:
  Bad annotation for Sx: NeL
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx L NeP Rfi
  diyone7: Fatal error: Annotations mismatch between L NeP.
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NeL LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwnelp-rfipnel [LxSxNeLP RfiPNeL] failed:
  Bad annotation for Lx: NeL
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx NePa Rfi
  diyone7: Fatal error: Test CoRW1+rmwpnepa-rfinepap [LxSxPNePa RfiNePaP] failed:
  Bad annotation for Sx: NePa
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePa LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwnepap-rfipnepa [LxSxNePaP RfiPNePa] failed:
  Bad annotation for Lx: NePa
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx NePaN Rfi
  diyone7: Fatal error: Test CoRW1+rmwpnepan-rfinepanp [LxSxPNePaN RfiNePaNP] failed:
  Bad annotation for Sx: NePaN
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc NePaN LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwnepanp-rfipnepan [LxSxNePaNP RfiPNePaN] failed:
  Bad annotation for Lx: NePaN
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne1 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne1-rfine1p [LxSxPNe1 RfiNe1P] failed:
  Bad annotation for Sx: Ne1
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne1 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne1p-rfipne1 [LxSxNe1P RfiPNe1] failed:
  Bad annotation for Lx: Ne1
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne2 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne2-rfine2p [LxSxPNe2 RfiNe2P] failed:
  Bad annotation for Sx: Ne2
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne2p-rfipne2 [LxSxNe2P RfiPNe2] failed:
  Bad annotation for Lx: Ne2
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne3 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne3-rfine3p [LxSxPNe3 RfiNe3P] failed:
  Bad annotation for Sx: Ne3
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne3p-rfipne3 [LxSxNe3P RfiPNe3] failed:
  Bad annotation for Lx: Ne3
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne4 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne4-rfine4p [LxSxPNe4 RfiNe4P] failed:
  Bad annotation for Sx: Ne4
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne4p-rfipne4 [LxSxNe4P RfiPNe4] failed:
  Bad annotation for Lx: Ne4
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne2i Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne2i-rfine2ip [LxSxPNe2i RfiNe2iP] failed:
  Bad annotation for Sx: Ne2i
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne2i LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne2ip-rfipne2i [LxSxNe2iP RfiPNe2i] failed:
  Bad annotation for Lx: Ne2i
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne3i Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne3i-rfine3ip [LxSxPNe3i RfiNe3iP] failed:
  Bad annotation for Sx: Ne3i
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne3i LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne3ip-rfipne3i [LxSxNe3iP RfiPNe3i] failed:
  Bad annotation for Lx: Ne3i
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc P LxSx Ne4i Rfi
  diyone7: Fatal error: Test CoRW1+rmwpne4i-rfine4ip [LxSxPNe4i RfiNe4iP] failed:
  Bad annotation for Sx: Ne4i
  
  [2]

  $ diyone7 -arch AArch64 -variant neon -metadata false -oneloc Ne4i LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwne4ip-rfipne4i [LxSxNe4iP RfiPNe4i] failed:
  Bad annotation for Lx: Ne4i
  
  [2]

# Morello annotations

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P LxSx Pc Rfi
  AArch64 CoRW1+rmwppc-rfipcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0              ;
   MOV X2,#1       ;
   Loop00:         ;
   LDXR X1,[C0]    ;
   STXR W3,C2,[X0] ;
   CBNZ X3,Loop00  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc LxSx P Rfi
  AArch64 CoRW1+rmwpcp-rfippc
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0              ;
   MOV X2,#1       ;
   Loop00:         ;
   LDXR C1,[C0]    ;
   GCVALUE X1,C1   ;
   STXR W3,X2,[X0] ;
   CBNZ X3,Loop00  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P LxSx Ac Rfi
  diyone7: Fatal error: Test CoRW1+rmwpac-rfiacp [LxSxPAc RfiAcP] failed:
  Bad annotation for Sx: Ac
  
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ac LxSx P Rfi
  AArch64 CoRW1+rmwacp-rfipac
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0              ;
   MOV X2,#1       ;
   Loop00:         ;
   LDAXR C1,[C0]   ;
   GCVALUE X1,C1   ;
   STXR W3,X2,[X0] ;
   CBNZ X3,Loop00  ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Pc A LxSx P Rfi
  diyone7: Fatal error: Invalid extra annotation Pc
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P LxSx Lc Rfi
  AArch64 CoRW1+rmwplc-rfilcp
  Variant=morello
  {
   __uint128 x=0;
   0:X0=0xffffc0000:x:1;
  }
   P0               ;
   MOV X2,#1        ;
   Loop00:          ;
   LDXR X1,[C0]     ;
   STLXR W3,C2,[X0] ;
   CBNZ X3,Loop00   ;
  
  exists (0:X1=1)

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P LxSx L Pc Rfi
  diyone7: Fatal error: Annotations mismatch between L Pc.
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Lc LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwlcp-rfiplc [LxSxLcP RfiPLc] failed:
  Bad annotation for Lx: Lc
  
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P LxSx Ct Rfi
  diyone7: Fatal error: Test CoRW1+rmwpct-rfictp [LxSxPCt RfiCtP] failed:
  variant annotation on xstore
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Ct LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwctp-rfipct [LxSxCtP RfiPCt] failed:
  variant annotation on xload
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc P LxSx Cs Rfi
  diyone7: Fatal error: Test CoRW1+rmwpcs-rficsp [LxSxPCs RfiCsP] failed:
  variant annotation on xstore
  [2]

  $ diyone7 -arch AArch64 -variant morello -metadata false -oneloc Cs LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwcsp-rfipcs [LxSxCsP RfiPCs] failed:
  variant annotation on xload
  [2]

# Ifetch annotations

  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc P LxSx I Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

  $ diyone7 -arch AArch64 -variant ifetch -metadata false -oneloc I LxSx P Rfi
  diyone7: Fatal error: Invalid extra annotation I
  [2]

# MemTag annotations

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc P LxSx T Rfi
  diyone7: Fatal error: Test CoRW1+rmwpt-rfitp [LxSxPT RfiTP] failed:
  variant annotation on xstore
  [2]

  $ diyone7 -arch AArch64 -variant memtag -metadata false -oneloc T LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwtp-rfipt [LxSxTP RfiPT] failed:
  variant annotation on xload
  [2]

# VMSA annotations

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx Pte Rfi
  diyone7: Fatal error: Test CoRW1+rmwppte-rfiptep [LxSxPPte RfiPteP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwptep-rfippte [LxSxPteP RfiPPte] failed:
  Bad annotation for Lx: Pte
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx Pte A Rfi
  diyone7: Fatal error: Test CoRW1+rmwpptea-rfipteap [LxSxPPteA RfiPteAP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte A LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpteap-rfipptea [LxSxPteAP RfiPPteA] failed:
  Bad annotation for Lx: PteA
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx Pte Q Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteq-rfipteqp [LxSxPPteQ RfiPteQP] failed:
  Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte Q LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpteqp-rfippteq [LxSxPteQP RfiPPteQ] failed:
  AcqPC annotation on xload
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteOA Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteoa-rfipteoap [LxSxPPteOA RfiPteOAP] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteOA LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpteoap-rfippteoa [LxSxPteOAP RfiPPteOA] failed:
  Bad annotation for Lx: PteOA
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteV1 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpptev1-rfiptev1p [LxSxPPteV1 RfiPteV1P] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteV1 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwptev1p-rfipptev1 [LxSxPteV1P RfiPPteV1] failed:
  Bad annotation for Lx: PteV1
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteAF0 Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteaf0-rfipteaf0p [LxSxPPteAF0 RfiPteAF0P] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteAF0 LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpteaf0p-rfippteaf0 [LxSxPteAF0P RfiPPteAF0] failed:
  Bad annotation for Lx: PteAF0
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteHA Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteha-rfiptehap [LxSxPPteHA RfiPteHAP] failed:
  Bad annotation for Sx: PteHA
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHA LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwptehap-rfippteha [LxSxPteHAP RfiPPteHA] failed:
  Bad annotation for Lx: PteHA
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteHD Rfi
  diyone7: Fatal error: Test CoRW1+rmwpptehd-rfiptehdp [LxSxPPteHD RfiPteHDP] failed:
  Bad annotation for Sx: PteHD
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc PteHD LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwptehdp-rfipptehd [LxSxPteHDP RfiPPteHD] failed:
  Bad annotation for Lx: PteHD
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteV1 PteAF0 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpptev1.af0-rfiptev1.af0p [LxSxPPteV1.AF0 RfiPteV1.AF0P] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteV1 PteOA Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteoa.v1-rfipteoa.v1p [LxSxPPteOA.V1 RfiPteOA.V1P] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteAF0 PteOA Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteoa.af0-rfipteoa.af0p [LxSxPPteOA.AF0 RfiPteOA.AF0P] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteV1 PteAF0 PteOA Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteoa.v1.af0-rfipteoa.v1.af0p [LxSxPPteOA.V1.AF0 RfiPteOA.V1.AF0P] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx PteHA PteHD Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteha.hd-rfipteha.hdp [LxSxPPteHA.HD RfiPteHA.HDP] failed:
  Bad annotation for Sx: PteHA.HD
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A Pte LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwpteap-rfipptea [LxSxPteAP RfiPPteA] failed:
  Bad annotation for Lx: PteA
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc A PteHA LxSx P Rfi
  diyone7: Fatal error: Test CoRW1+rmwptehaap-rfipptehaa [LxSxPteHAAP RfiPPteHAA] failed:
  Bad annotation for Lx: PteHAA
  
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx L PteOA Rfi
  diyone7: Fatal error: Test CoRW1+rmwppteoal-rfipteoalp [LxSxPPteOAL RfiPteOALP] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc P LxSx L PteV1 PteAF0 Rfi
  diyone7: Fatal error: Test CoRW1+rmwpptev1.af0l-rfiptev1.af0lp [LxSxPPteV1.AF0L RfiPteV1.AF0LP] failed:
  Cannot convert to int
  [2]

  $ diyone7 -arch AArch64 -variant vmsa -metadata false -oneloc Pte X LxSx P Rfi
  diyone7: Fatal error: Invalid extra annotation Pte
  [2]
