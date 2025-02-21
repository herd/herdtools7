func Increment(x: integer) => integer
begin
  return x + 1;
end;

func DoubleBitvectorLength{N}(x: bits(N)) => integer{2*N}
begin
  return 2*N;
end;

func main () => integer
begin
  let x : integer               = Increment(41);
  assert x == 42;
  let y : integer{30}           = DoubleBitvectorLength{15}(Zeros{15});
  assert y == 30;
  return 0;
end;
