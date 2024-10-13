type MyBV of bits(8) { [5:] bitfield };

getter F => MyBV
begin
  return Zeros(8) as MyBV;
end

setter F = v: MyBV
begin
  assert v[0:] == '0';
end

func foo () => (bit, integer)
begin
  return ('0', 1);
end

func main () => integer
begin
  var x = 0;
  (F.bitfield, x) = foo();
  assert x == 1;

  return 0;
end

