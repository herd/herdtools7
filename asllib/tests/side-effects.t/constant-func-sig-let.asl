let K : integer {8, 16, 32} = 8;

pure func foo(x: integer {0..K}) => integer
begin
  return x;
end;

constant C = foo(3);

func main () => integer
begin
  assert C == 3;

  return 0;
end;

