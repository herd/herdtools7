pure func foo (n: integer) => integer recurselimit 5
begin
  if n <= 0 then
    return 1;
  else
    return bar (n - 1);
  end;
end;

pure func bar (n: integer) => integer
begin
  let r = foo (1);
  return n * r * foo (n);
end;

constant C = foo (4);
constant D: integer = bar (5);

func main () => integer
begin
  assert C == 6;
  assert D == 120;

  return 0;
end;

