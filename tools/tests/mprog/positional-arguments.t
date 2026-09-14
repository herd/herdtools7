  $ cat >A1.litmus <<EOF
  > AArch64 A1
  > {}
  > P0  ;
  > NOP ;
  > EOF

  $ sed 's/A1/A2/' A1.litmus >A2.litmus
  $ sed 's/A1/A3/' A1.litmus >A3.litmus

Positional arguments are processed in the order they are specified on the
command line.

  $ mprog7 -mode text A1.litmus - A2.litmus <A3.litmus | grep AArch64
  AArch64 A1
  AArch64 A3
  AArch64 A2
