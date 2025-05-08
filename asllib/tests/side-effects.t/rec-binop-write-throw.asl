type E of exception {-};
var X: integer = 0;

func throwing (n: integer, b: boolean) => integer
begin
  if b then
    throw E {-};
  else
    return foo (n);
  end;
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
  let x = throwing (n - 1, FALSE) * write_X ();
  return 2 * x;
end;

func main () => integer
begin
  let x = foo (4);

  return 0;
end;
