type MyBV of bits(8) { [5] bitfield };

accessor F() <=> MyBV
begin
  getter begin
    return Zeros{8} as MyBV;
  end;

  setter = v begin
    assert v[0] == '0';
  end;
end;

func main () => integer
begin
  let res = F().bitfield;
  assert res == '0';

  return 0;
end;

