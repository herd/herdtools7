var X: integer = 0;

pure func foo (x: integer) => integer
begin
  X = 3;
  return x * x + 3;
end;

constant C = foo (4);

func main () => integer
begin
  assert C == 19;

  return 0;
end;

