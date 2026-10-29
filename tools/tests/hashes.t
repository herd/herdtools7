Apply mshowhashes7 and mrehash7 so as to test lexers
in the case where the test starts with a comment.
Also test processing order of tokens from the command line.
  $ TEST=../../gen/tests/AArch64/MP.litmus
  $ NEXT=../../gen/tests/AArch64/MP+po+addr.litmus
  $ mshowhashes7 $TEST $NEXT| tee H
  MP 5a017e93dc4005149f270cbd05688a55
  MP+po+addr f50a40c975e8ea58c6132797bad1c844
  $ rehash7 -hashes H $NEXT $TEST
  (* diyone7 -arch AArch64 -metadata false PodWW Rfe DpAddrdR Fre *)
  AArch64 MP+po+addr
  Hash=f50a40c975e8ea58c6132797bad1c844
  {
   0:X1=x; 0:X3=y;
   1:X1=x; 1:X3=y;
  }
   P0          | P1                  ;
   MOV W0,#1   | LDR W0,[X3]         ;
   STR W0,[X1] | EOR W2,W0,W0        ;
   MOV W2,#1   | LDR W4,[X1,W2,SXTW] ;
   STR W2,[X3] |                     ;
  
  exists (1:X0=1 /\ 1:X4=0)
  (* diyone7 -arch AArch64 -metadata false PodWW Rfe PodRR Fre *)
  AArch64 MP
  Hash=5a017e93dc4005149f270cbd05688a55
  {
   0:X1=x; 0:X3=y;
   1:X1=x; 1:X3=y;
  }
   P0          | P1          ;
   MOV W0,#1   | LDR W0,[X3] ;
   STR W0,[X1] | LDR W2,[X1] ;
   MOV W2,#1   |             ;
   STR W2,[X3] |             ;
  
  exists (1:X0=1 /\ 1:X2=0)

