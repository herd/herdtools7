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
          assert k MOD 2^(m+1) == 2^m;
                 ^^^^^^^^^^^^^
  Warning: Removing some values that would fail with op MOD from constraint set
  {0..(2 ^ (n + 1)), 1, (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))} gave
  {1, 1..(2 ^ (n + 1)), (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))}. Continuing
  with this constraint set.

  $ aslref rotate.asl
  V = '100'
  
  ROR(V,0) = '100'
  ROR(V,1) = '010'
  ROR(V,2) = '001'
  ROR(V,3) = '100'
  
  ROR_C(V,1) = ('010', '0')
  ROR_C(V,2) = ('001', '0')
  ROR_C(V,3) = ('100', '1')
  ROR_C(V,4) = ('010', '0')
  
  ROL(V,0) = '100'
  ROL(V,1) = '001'
  ROL(V,2) = '010'
  ROL(V,3) = '100'
  
  ROL_C(V,1) = ('001', '1')
  ROL_C(V,2) = ('010', '0')
  ROL_C(V,3) = ('100', '0')
  ROL_C(V,4) = ('001', '1')
  

  $ aslref misc.asl

Checking that --no-primitives option actually removes OCaml primitives
(different errors are produced)
  $ aslref no-primitives-test.asl
  ASL Execution error: Mismatch type: value -1 does not belong to type integer.
  [1]
  $ aslref --no-primitives no-primitives-test.asl
  File ASL Standard Library, line 60, characters 11 to 16:
  ASL Execution error: Assertion failed: (__stdlib_local_a > 0).
  [1]

Tests using ASL stdlib only
  $ aslref --no-primitives uint.asl
  $ aslref --no-primitives sint.asl
  $ aslref --no-primitives pow2.asl
  $ aslref --no-primitives log2.asl
  $ aslref --no-primitives ilog2.asl
  $ aslref --no-primitives align.asl
  $ aslref --no-primitives bits.asl
  $ aslref --no-primitives sqrt.asl
  $ aslref --no-primitives round.asl
  $ aslref --no-primitives set-bits.asl
  File set-bits.asl, line 29, characters 15 to 28:
          assert k MOD 2^(m+1) == 2^m;
                 ^^^^^^^^^^^^^
  Warning: Removing some values that would fail with op MOD from constraint set
  {0..(2 ^ (n + 1)), 1, (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))} gave
  {1, 1..(2 ^ (n + 1)), (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))}. Continuing
  with this constraint set.

  $ aslref --no-primitives rotate.asl
  V = '100'
  
  ROR(V,0) = '100'
  ROR(V,1) = '010'
  ROR(V,2) = '001'
  ROR(V,3) = '100'
  
  ROR_C(V,1) = ('010', '0')
  ROR_C(V,2) = ('001', '0')
  ROR_C(V,3) = ('100', '1')
  ROR_C(V,4) = ('010', '0')
  
  ROL(V,0) = '100'
  ROL(V,1) = '001'
  ROL(V,2) = '010'
  ROL(V,3) = '100'
  
  ROL_C(V,1) = ('001', '1')
  ROL_C(V,2) = ('010', '0')
  ROL_C(V,3) = ('100', '0')
  ROL_C(V,4) = ('001', '1')
  

  $ aslref --no-primitives misc.asl

