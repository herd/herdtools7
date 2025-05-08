func main () => integer
begin
  // Pattern matching against bitmasks.
  // All of the following pattern matching expressions are equivalent:
  let bv = '11010';
  assert   bv IN {'11xx0'};
  assert   bv IN {'11(00)0'};
  assert   bv IN {'11(01)0'};
  assert   bv IN {'11(10)0'};
  assert   bv IN {'11(11)0'};
  assert   bv IN {'11(00)0'};
  assert   bv IN {'11(01)0'};
  assert   bv IN {'11(10)0'};
  assert   bv IN {'11(11)0'};
  assert   bv IN {'11(1)x0'};
  assert   bv == '11xx0';
  assert   bv == '11(00)0';
  assert   bv == '11x(0)0';

  assert !(bv IN {'11x00'});
  assert !(bv IN {'11(00)1'});

  let match_true = 42 IN {0..3, 42};
  assert match_true == TRUE;

  let match_false = 42 IN {0..3, -4};
  assert match_false == FALSE;

  return 0;
end;
