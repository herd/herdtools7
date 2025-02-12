type MyBV of bits(8) { [5] bitfield };

accessor F() <=> MyBV
begin
  getter begin
    return Ones{8} as MyBV;
  end;

  setter = v begin
    assert v.bitfield == '0';
  end;
end;

func main () => integer
begin
  let res = F().bitfield;
  assert res == '1';
  F().bitfield = '0';

  return 0;
end;

