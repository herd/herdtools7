var X: integer = 0;

func not_throwing (n: integer) => integer
begin
  return foo (n);
end;

func write_X () => integer
begin
  let x = X;
  X = x + 1;
  return x;
end;

func foo (n: integer) => integer
begin
  if n <= 0 then return 0; end;
  let x = not_throwing (n - 1) * write_X ();
  return 2 * x;
end;

func main () => integer
begin
  let x = foo (4);

  return 0;
end;

