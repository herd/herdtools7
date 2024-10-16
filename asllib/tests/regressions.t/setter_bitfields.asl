type MyBV of bits(8) { [5] bitfield };

getter F() => MyBV
begin
  return Ones{8} as MyBV;
end;

setter F() = v: MyBV
begin
  assert v.bitfield == '0';
end;

func main () => integer
begin
  let res = F().bitfield;
  assert res == '1';
  F().bitfield = '0';

  return 0;
end;

