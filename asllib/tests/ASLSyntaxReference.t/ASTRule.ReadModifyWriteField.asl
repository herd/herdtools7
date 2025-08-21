type MyRecord of record {val: integer};

var r: MyRecord;

accessor F() <=> new_r: MyRecord
begin
  readonly getter
    return r;
  end;

  setter
    r = new_r;
  end;
end;

func main () => integer
begin
  F().val = 5;

  // The above is desugared into the following
  // (with an appropriate fresh name for 'setter_v1_temp'):
  var setter_v1_temp = F();
  setter_v1_temp.val = 5;
  F() = setter_v1_temp;

  return 0;
end;
