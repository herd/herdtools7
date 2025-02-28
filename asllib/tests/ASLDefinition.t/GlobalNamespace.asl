var X : integer = 0;

func X() => integer
begin
  return X;
end;

func main() => integer
begin
  X = X() + 1;
  assert X() == 1;

  return 0;
end;
