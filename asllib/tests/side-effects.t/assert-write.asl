var X: integer = 0;

func write_X () => integer
begin
  let x = X;
  X = x + 1;
  return x;
end;

func main () => integer
begin
  assert write_X () == 0;

  return 0;
end;

