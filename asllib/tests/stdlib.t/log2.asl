func main () => integer
begin
  assert Log2(1) == 0;
  assert Log2(2) == 1;
  assert Log2(3) == 1;

  for n = 2 to 25 do
    assert Log2(2 ^ n - 1) == n - 1;
    assert Log2(2 ^ n) == n;
    assert Log2(2 ^ n + 1) == n;
  end;

  for n = 1 to 1000 do
    assert 2 ^ Log2(n) <= n;
    assert 2 ^ (Log2(n) + 1) > n;
  end;

  return 0;
end;
