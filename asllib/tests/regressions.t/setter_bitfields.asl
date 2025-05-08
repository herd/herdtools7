type MyBV of bits(8) { [5] bitfield };

accessor F() <=> v: MyBV
begin
  getter
    return Ones{8} as MyBV;
  end;

  setter
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

