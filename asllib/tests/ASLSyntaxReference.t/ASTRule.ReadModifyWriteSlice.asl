type MyBV of bits(8);

var bv: MyBV;

accessor F() <=> new_bv: MyBV
begin
  readonly getter
    return bv;
  end;

  setter
    bv = new_bv;
  end;
end;

func main () => integer
begin
  F()[2+:1] = '1';

  // The above is desugared into the following
  // (with an fresh name instead of 'setter_v1_temp'):
  var setter_v1_temp = F();
  setter_v1_temp[2+:1] = '1';
  F() = setter_v1_temp;

  return 0;
end;
