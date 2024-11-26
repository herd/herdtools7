
func main () => integer
begin
  let x = 64 << UInt(ARBITRARY: bits(1)); // integer {64, 128}
  let y = 8 << UInt(ARBITRARY: bits(2)); // integer {8, 16, 32, 64}
  let bv = ARBITRARY: bits(y);

  let bv2: bits(x) = Replicate{}(bv);

  return 0;
end;
