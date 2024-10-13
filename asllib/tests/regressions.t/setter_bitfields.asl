type MyBV of bits(8) { [5:] bitfield };

getter F[] => MyBV
begin
  return Zeros(8) as MyBV;
end

getter F[field: integer] => bit
begin
  assert field == 5;
  return Ones(1);
end

setter F[field: integer] = v: bit
begin
  assert field == 5;
  assert v == '0';
end

func main () => integer
begin
  assert F.bitfield == '1';
  F.bitfield = '0';

  return 0;
end

