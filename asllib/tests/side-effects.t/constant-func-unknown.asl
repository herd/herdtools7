pure func foo (x: integer) => integer
begin
  let y = ARBITRARY: integer {0..3};
  return x * x + 3 + y;
end;

constant C = foo (4);

func main () => integer
begin
  assert C == 19;

  return 0;
end;

