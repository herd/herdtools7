var X: integer = 2;

func not_throwing (n: integer) => integer
begin
  return foo (n);
end;

func read_X () => integer
begin
  let x = X;
  return x;
end;

func foo (n: integer) => integer
begin
  if n <= 0 then return 1; end;
  let x = not_throwing (n - 1) * read_X ();
  return 2 * x;
end;

func main () => integer
begin
  let x = foo (4);

  return 0;
end;

