func bar (n: integer) => integer
begin
  return foo (n);
end;

func foo (n: integer) => integer
begin
  if n <= 0 then return 1; end;
  let x = bar (n - 1) * bar (n - 2);
  return 2 * x;
end;

func main () => integer
begin
  let x = foo (4);

  return 0;
end;


