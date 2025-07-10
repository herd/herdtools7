integer f(integer x)
  if x >= 0 then
    return x;
  else
    Unreachable();

integer main()
  integer f1 = f(1);
  assert f1 == 1;
  return 0;
