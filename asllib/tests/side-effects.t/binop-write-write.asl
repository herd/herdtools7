var X: integer = 0;

func set_and_return () => integer
begin
  X = 2;
  return 3;
end;

func main () => integer
begin
  let y = set_and_return () + set_and_return ();

  return 0;
end;
