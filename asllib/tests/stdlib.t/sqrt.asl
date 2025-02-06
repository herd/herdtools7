func main () => integer
begin
  for i = 1 to 100 do
    let x = Real(i);
    let expected_res = SqrtRounded(x, 1000);
    for p = 1 to 10 do
      let res = SqrtRounded(x, p);
      assert Abs(res - expected_res) / expected_res < 2.0 ^ (-p);
    end;
  end;

  return 0;
end;

