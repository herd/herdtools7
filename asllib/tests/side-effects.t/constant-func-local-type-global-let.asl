let K : integer {8, 16, 32} = 8;

pure func foo(x: integer) => integer
begin
  let k = x as integer {K};

  assert k == x;

  return 2 * k;
end;

constant C = foo (8);

func main () => integer
begin
  assert C == 16;

  return 0;
end;
