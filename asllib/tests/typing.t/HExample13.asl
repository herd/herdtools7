
func main () => integer
begin
  let x = 64 << UInt(UNKNOWN: bits(1)); // integer {64, 128}
  let y = 8 << UInt(UNKNOWN: bits(2)); // integer {8, 16, 32, 64}
  let bv = UNKNOWN: bits(y);

  let bv2: bits(x) = Replicate{x DIV y,y}(bv);

  return 0;
end;
