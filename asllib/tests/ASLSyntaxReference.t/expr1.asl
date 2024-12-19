type point of record{x: bits(4), y: bits(4)};
type except of exception;

func main() => integer
begin
  var v: integer = 4;
  // E_Var: v is a variable expression.
  var - = v;

  var b0 = '1111 1000'[3:1, 0]; // E_Slice 1: a bitvector slice.
  var b1 = 0xF8[3:1, 0]; // E_Slice 2: an integer slice.
  var bits_arr : array [1] of bits(4);
  // E_Binop 1: b0 == b1 is a binary expression for ==.
  // E_Cond 1: the right-hand side of the assignment is
  //           a conditional expression.
  bits_arr[[0]] = if (b0 == b1) then '1000' else '0000';
  // E_Slice 3: bits_arr[0] stands for an array access
  assert b0 == bits_arr[[0]];
  // E_Unop 1: (NOT b8) negates the bits of b8.
  // E_Binop 2: the right-hand side of the assignment is
  //            a binay AND expression.
  // E_Concat 1: b0 :: b1 concatenates two bitvectors.
  // E_Arbitrary 1: ARBITRARY: bits(8) represents an arbitrary
  //                8-bits bitvector
  var b8 = b0 :: b1;
  b8 = (NOT b8) AND ARBITRARY: bits(8);
  return 0;
end;
