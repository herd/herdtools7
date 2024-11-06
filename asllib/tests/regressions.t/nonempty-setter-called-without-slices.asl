getter f1() => integer
begin
  return 4;
end;

setter f1() = v: integer
begin
  pass;
end;

func main () => integer
begin
  f1 = 4;

  return 0;
end;
