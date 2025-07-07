pure func foo (n: integer) => integer
begin
  if n <= 0 then
    return 1;
  else
    return bar (n);
  end;
end;

pure func bar (n: integer) => integer recurselimit 1000
begin
  return n * foo (n - 1);
end;

constant C = foo (4);
constant D: integer = bar (5);

func main () => integer
begin
  assert C == 24;
  assert D == 120;

  return 0;
end;
