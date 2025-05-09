func main () => integer
begin
  for i = 1 to 1000 do
    let x = Real (i) / 100.0;
    assert (Abs(ILog2(x) + ILog2(1.0 / x)) < 2);
  end;

  for i = -10 to 10 do
    let x = 3.0 ^ i;
    let lgx = ILog2(x);
    assert lgx == ILog2(-x);
    assert (Abs(lgx + ILog2(1.0 / x)) < 2);
    if i >= 0 then
      assert FloorLog2(3 ^ (i as integer)) == lgx;
    end;
  end;

  for i = 10 to 1000 do
    assert FloorLog2(i DIVRM 10) == ILog2 (Real (i) / 10.0);
  end;

  return 0;
end;
