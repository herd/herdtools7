type MyBV of bits(8) { [5] b1, [4] b2 };

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
  let res = F().[b1, b2];
  assert res == '00';

  return 0;
end;

