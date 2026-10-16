Tests using ASLRef OCaml primitives for some stdlib functions
  $ aslref uint.asl
  $ aslref sint.asl
  $ aslref sint_zero_static.asl
  : All values in constraints {-1} would fail with op ^, operation will always
  fail.
  ASL Type error (TE_BO): Illegal application of operator ^ on types
    integer {2} and integer {-1}.
  [1]
  $ aslref sint_zero_dynamic.asl
  ASL Warning: Removing some values that would fail with op ^ from constraint
  set {-1..0} gave {0..0}. Continuing with this constraint set.
  ASL Warning: Removing some values that would fail with op ^ from constraint
  set {-1..0} gave {0..0}. Continuing with this constraint set.
  ASL Dynamic error (DE_DAF):
    SInt (primitive) expected an argument length greater than 0
  [1]
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
  ASL Warning: Removing some values that would fail with op MOD from constraint
  set {0..(2 ^ (n + 1)), 1, (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))} gave
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
  ASL Dynamic error (DE_DAF):
    FloorLog2 (primitive) expected an argument greater than 0
  [1]
  $ aslref --no-primitives no-primitives-test.asl
  File ASL Standard Library, line 80, characters 11 to 16:
  ASL Dynamic error (DE_DAF): Assertion failed: (__stdlib_local_a > 0).
  [1]

Tests using ASL stdlib only
  $ aslref --no-primitives uint.asl
  $ aslref --no-primitives sint.asl
  $ aslref --no-primitives sint_zero_static.asl
  File ASL Standard Library, line 33, characters 44 to 51: All values in
  constraints {-1} would fail with op ^, operation will always fail.
  File ASL Standard Library, line 33, characters 44 to 51:
  ASL Type error (TE_BO): Illegal application of operator ^ on types
    integer {2} and integer {-1}.
  [1]
  $ aslref --no-primitives sint_zero_dynamic.asl
  File ASL Standard Library, line 33, characters 44 to 51:
  ASL Warning: Removing some values that would fail with op ^ from constraint
  set {-1..0} gave {0..0}. Continuing with this constraint set.
  File ASL Standard Library, line 33, characters 57 to 64:
  ASL Warning: Removing some values that would fail with op ^ from constraint
  set {-1..0} gave {0..0}. Continuing with this constraint set.
  File ASL Standard Library, line 35, characters 11 to 16:
  ASL Dynamic error (DE_DAF): Assertion failed: (__stdlib_local_N > 0).
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
          assert k MOD 2^(m+1) == 2^m;
                 ^^^^^^^^^^^^^
  ASL Warning: Removing some values that would fail with op MOD from constraint
  set {0..(2 ^ (n + 1)), 1, (- ((- 2) ^ (n + 1)))..((- 2) ^ (n + 1))} gave
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

