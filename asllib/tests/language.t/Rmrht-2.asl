//R_MRHT: If the arguments of a comparison operation are bitvectors then
//they must have the same determined width.

// RUN: not interp %s | FileCheck %s

func compare(int1: integer {1,2},
             int2: integer {4,5},
             bit1: bits(int1),
             bit2: bits(int2),
             int3: integer {1,2},
             bit3: bits(int3)
             )

begin
  var cond: boolean;
  cond = int1 == int2; // Legal
  cond = bit1 == bit2; // Illegal
  cond = bit1 == bit3; // Illegal
  // Type check failure since type checker knows
  // int1 ==> int1
  // int3 ==> int3
  // and cannot prove they are always the same
end
