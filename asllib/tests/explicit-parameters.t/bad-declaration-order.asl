// Parameters are declared as they appear textually left-to-right in return
// type then argument types

func Bad{D,E,A,B,C}(x: bits(D), y: bits(E)) => bits(A * B + C)
begin
  return Zeros{A * B + C};
end;

func main() => integer
begin
  return 0;
end;
