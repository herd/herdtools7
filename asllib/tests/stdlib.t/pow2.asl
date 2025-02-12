func check_pow2 (n: integer)
begin
  assert n > 1;

  let p = 2 ^ n;

  assert FloorPow2(p-1) == p DIVRM 2;
  assert  CeilPow2(p-1) == p;
  assert    IsPow2(p-1) == FALSE;

  assert FloorPow2(p) == p;
  assert  CeilPow2(p) == p;
  assert    IsPow2(p) == TRUE;

  assert FloorPow2(p+1) == p;
  assert  CeilPow2(p+1) == 2 * p;
  assert    IsPow2(p+1) == FALSE;
end;

func main () => integer
begin
  assert    IsPow2(0) == FALSE;
  assert  CeilPow2(0) == 1;
  // No FloorPow2 for 0

  assert    IsPow2(1) == TRUE;
  assert  CeilPow2(1) == 1;
  assert FloorPow2(1) == 1;

  assert    IsPow2(2) == TRUE;
  assert  CeilPow2(2) == 2;
  assert FloorPow2(2) == 2;

  assert    IsPow2(3) == FALSE;
  assert  CeilPow2(3) == 4;
  assert FloorPow2(3) == 2;

  for n = 2 to 10 do
    check_pow2(n);
  end;

  for n = 1 to 10 do
    check_pow2(19*n+1);
  end;

  return 0;
end;
