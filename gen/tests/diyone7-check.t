valid-cycle-with-p-annotation
  $ diyone7 -arch AArch64 PodWR P Fre PodWR Fre
  AArch64 SB
  "PodWR Fre PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWR Fre PodWR Fre
  {
  0:X1=x; 0:X2=y;
  1:X2=y; 1:X1=x;
  }
   P0          | P1          ;
   MOV W0,#1   | MOV W0,#1   ;
   STR W0,[X1] | STR W0,[X2] ;
   LDR W3,[X2] | LDR W3,[X1] ;
  exists (0:X3=0 /\ 1:X3=0)
valid-cycle-with-duplicate-annotations
  $ diyone7 -arch AArch64 PodWR A A Fre PodWR Fre
  AArch64 SB+po+popa
  "PodWRPA FreAP PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA FreAP PodWR Fre
  {
  0:X1=x; 0:X2=y;
  1:X2=y; 1:X1=x;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   LDAR W3,[X2] | LDR W3,[X1] ;
  exists (0:X3=0 /\ 1:X3=0)
  $ diyone7 -arch AArch64 PodWR A A A Fre PodWR Fre
  AArch64 SB+po+popa
  "PodWRPA FreAP PodWR Fre"
  Generator=diyone7 (version 7.58+1)
  Prefetch=0:x=F,0:y=T,1:y=F,1:x=T
  Com=Fr Fr
  Orig=PodWRPA FreAP PodWR Fre
  {
  0:X1=x; 0:X2=y;
  1:X2=y; 1:X1=x;
  }
   P0           | P1          ;
   MOV W0,#1    | MOV W0,#1   ;
   STR W0,[X1]  | STR W0,[X2] ;
   LDAR W3,[X2] | LDR W3,[X1] ;
  exists (0:X3=0 /\ 1:X3=0)
invalid-cycle-with-incorrect-annotations
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
