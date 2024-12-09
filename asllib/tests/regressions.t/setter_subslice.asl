type MyBV of bits(8) { [5] bitfield };

getter F () => MyBV
begin
  return Zeros{8} as MyBV;
end;

setter F () = v: MyBV
begin
  assert v[0] == '0';
end;

func main () => integer
begin
  F()[2+:1] = '1';

  return 0;
end;

