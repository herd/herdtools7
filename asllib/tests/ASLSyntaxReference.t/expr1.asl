getter g_no_args => integer begin return 0; end

getter g0_bits[] => bits(4) begin return '1000'; end

getter g1_bits[p: integer] => bits(4)
begin
  return '1000'[p:, 2:0];
end

type point of record{x: bits(4), y: bits(4)};
type except of exception;

func main() => integer
begin
  var v: integer = 4;
  // E_Var 1: v is a variable expression.
  // E_Var 2: g_no_args is a call to a getter with no arguments.
  var - = v + g_no_args;

  var b0 = '1111 1000'[3:1, 0:]; // E_Slice 1: a bitvector slice.
  var b1 = 0xF8[3:1, 0:]; // E_Slice 2: an integer slice.
  // E_Slice 3: g0_bits[] is a call to a getter.
  assert b0 == g0_bits[];
  // E_Slice 4: g1_bits[3] is a call to a getter.
  assert b0 == g1_bits[3];
  var bits_arr : array [1] of bits(4);
  // E_Binop 1: b0 == b1 is a binary expression for ==.
  // E_Cond 1: the right-hand side of the assignment is
  //           a conditional expression.
  bits_arr[0] = if (b0 == b1) then '1000' else '0000';
  // E_Slice 5: bits_arr[0] stands for an array access
  assert b0 == bits_arr[0];
  // E_Unop 1: (NOT b8) negates the bits of b8.
  // E_Binop 2: the right-hand side of the assignment is
  //            a binay AND expression.
  // E_Concat 1: [b0, b1] concatenates two bitvectors.
  // E_Unknown 1: UNKNOWN: bits(8) represents an arbitrary
  //              8-bits bitvector
  var b8 = b0 :: b1;
  b8 = (NOT b8) AND UNKNOWN: bits(8);
  return 0;
end
