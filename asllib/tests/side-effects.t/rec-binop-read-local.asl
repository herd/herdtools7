func not_throwing (n: integer) => integer
begin
  return foo (n);
end;

func foo (n: integer) => integer
begin
  if n <= 0 then return 0; end;
  var y: integer = 1;
  let x = not_throwing (n - 1) * y;
  return 2 * x;
end;

func main () => integer
begin
  let x = foo (4);

  return 0;
end;


