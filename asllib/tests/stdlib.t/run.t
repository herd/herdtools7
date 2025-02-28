Tests using ASLRef OCaml primitives for some stdlib functions
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

Checking that --no-primitives option actually removes OCaml primitives
(different errors are produced)
  $ aslref no-primitives-test.asl
  ASL Execution error: Mismatch type: value 3 does not belong to type integer.
  [1]
  $ aslref --no-primitives no-primitives-test.asl
  File ASL Standard Library, line 69, characters 11 to 23:
  ASL Execution error: Assertion failed:
    (__stdlib_local_a == __stdlib_local_current).
  [1]

Tests using ASL stdlib only
  $ aslref --no-primitives uint.asl
  $ aslref --no-primitives sint.asl
  ASL Dynamic error: Illegal application of operator ^ for values 2 and -1.
  [1]
  $ aslref --no-primitives pow2.asl
  $ aslref --no-primitives log2.asl
  $ aslref --no-primitives ilog2.asl
  $ aslref --no-primitives align.asl
  $ aslref --no-primitives bits.asl
  $ aslref --no-primitives sqrt.asl
  $ aslref --no-primitives round.asl
  $ aslref --no-primitives set-bits.asl
  File set-bits.asl, line 29, characters 15 to 28:
  Warning: Removing some values that would fail with op MOD from constraint set
  {0..(2 ^ (n + 1)), 1, (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))} gave
  {1, 1..(2 ^ (n + 1)), (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))}. Continuing
  with this constraint set.

  $ aslref --no-primitives misc.asl

