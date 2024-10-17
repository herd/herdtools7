type MyBV of bits(8) { [5:] b1, [4:] b2 };

getter F => MyBV
begin
  return Zeros(8) as MyBV;
end

setter F = v: MyBV
begin
  assert v[0:] == '0';
end

func main () => integer
begin
  assert F.(b1, b2) == '00';

  return 0;
end

