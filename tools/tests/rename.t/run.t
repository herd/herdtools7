 Test rename default mode (and processing order)
  $ rename7 A.litmus B.litmus C.litmus D.litmus | grep AArch64
  AArch64 A
  AArch64 B
  AArch64 C
  AArch64 D
Test explicit suffixes mode
  $ rename7 -suffixes +BIS,+TER @all | grep AArch64
  AArch64 A
  AArch64 A+BIS
  AArch64 A+TER
  AArch64 D
Test numeric suffix mode
  $ cat @all | rename7 -addnum 2 | grep AArch64
  AArch64 A
  AArch64 A01
  AArch64 A02
  AArch64 D
Test rename mapping
  $ echo A B > N
  $ rename7 -rename N B.litmus
  AArch64 B
  { }
    P0       ;
   MOV W0,#2 ;
  forall 0:X0=2
