type E of exception {-};

func throwing (n: integer, b: boolean) => integer
begin
  if b then
    throw E {-};
  else
    return foo (n);
  end;
end;

func foo (n: integer) => integer
begin
  if n <= 0 then return 0; end;
  let x = throwing (n - 1, FALSE) * (2 as integer {3});
  return 2 * x;
end;

func main () => integer
begin
  let x = foo (4);

  return 0;
end;

