  $ aslref uint.asl
  $ aslref sint.asl
  $ aslref pow2.asl
  $ aslref log2.asl
  $ aslref ilog2.asl
  $ aslref align.asl
  $ aslref bits.asl
  $ aslref sqrt.asl
  $ aslref round.asl
  $ aslref set-bits.asl
  File set-bits.asl, line 29, characters 15 to 28:
  Warning: Removing some values that would fail with op MOD from constraint set
  {0..(2 ^ (n + 1)), 1, (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))} gave
  {1, 1..(2 ^ (n + 1)), (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))}. Continuing
  with this constraint set.

  $ aslref misc.asl


