type MyBV of bits(8) { [5] bitfield };

getter F => MyBV
begin
  return Zeros(8) as MyBV;
end

setter F = v: MyBV
begin
  assert v[0] == '0';
end

func main () => integer
begin
  let res = F[5];
  assert res == '0';

  return 0;
end

