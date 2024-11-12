func BadUndeclared(N: integer{3}) => bits(N)
begin
  return Zeros{N};
end;

func main() => integer
begin
  return 0;
end;
