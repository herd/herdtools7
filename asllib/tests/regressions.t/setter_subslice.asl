type MyBV of bits(8) { [5] bitfield };

accessor F() <=> v: MyBV
begin
  getter
    return Zeros{8} as MyBV;
  end;

  setter
    assert v[0] == '0';
  end;
end;

func main () => integer
begin
  F()[2+:1] = '1';

  return 0;
end;

