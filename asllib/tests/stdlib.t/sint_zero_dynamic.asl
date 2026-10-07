
pure func zero_or_one() => integer{0,1}
begin
  return 0;
end;

func main() => integer
begin
  let bv = Zeros{zero_or_one()};
  return SInt(bv);
end;
