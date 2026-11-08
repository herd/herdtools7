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
Same tests, with the -prefix option.
Default mode
  $ rename7 -prefix PLUS+ A.litmus B.litmus C.litmus D.litmus | grep AArch64
  AArch64 PLUS+A
  AArch64 PLUS+B
  AArch64 PLUS+C
  AArch64 PLUS+D
Explicit suffixes mode
  $ rename7 -prefix PLUS+ -suffixes +BIS,+TER @all | grep AArch64
  AArch64 PLUS+A
  AArch64 PLUS+A+BIS
  AArch64 PLUS+A+TER
  AArch64 PLUS+D
Numeric suffix mode
  $ cat @all | rename7 -prefix PLUS+ -addnum 2 | grep AArch64
  AArch64 PLUS+A
  AArch64 PLUS+A01
  AArch64 PLUS+A02
  AArch64 PLUS+D
Rename mapping
  $ echo A B > N
  $ rename7 -prefix PLUS+ -rename N B.litmus
  AArch64 PLUS+B
  { }
    P0       ;
   MOV W0,#2 ;
  forall 0:X0=2
Test the -prefix option. Notice that the filename is not prefixed.
  $ mkdir OUT && mkdir OUT/ONE && mkdir OUT/TWO
  $ rename7 -o OUT/ONE A.litmus
  $ rename7 -prefix PLUS+ -o OUT/TWO A.litmus
  $ ( cd OUT && ls */@all > @all )
  $ mnames7 OUT/@all
  OUT/ONE/A.litmus A
  OUT/TWO/A.litmus PLUS+A
