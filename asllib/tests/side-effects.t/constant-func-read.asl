var X: integer = 0;

pure func foo (x: integer) => integer
begin
  let y = X;
  return x * x + y;
end;

constant C = foo (4);

func main () => integer
begin
  assert C == 19;

  return 0;
end;

