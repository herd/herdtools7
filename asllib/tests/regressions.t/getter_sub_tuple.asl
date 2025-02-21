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
  let res = (F(), 3).item0;
  assert res == Zeros{8};

  return 0;
end;

