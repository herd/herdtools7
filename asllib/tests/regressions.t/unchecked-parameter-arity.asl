func f{N}(x : bits(N)) => integer begin
  return N;
end;

func main() => integer begin
  return f{8, 16}('00000000');
end;
