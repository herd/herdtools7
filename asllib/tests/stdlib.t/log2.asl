func main () => integer
begin
  for exp = 0 to 10 do
    assert FloorLog2(2^exp) == exp;
    assert CeilLog2(2^exp) == exp;

    for i = 2^exp + 1 to 2^(exp+1) - 1 do
      assert FloorLog2(i) == exp;
      assert CeilLog2(i) == exp + 1;
    end;
  end;

  return 0;
end;
