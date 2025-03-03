func main () => integer
begin
  assert Log2(1) == 0;
  assert Log2(2) == 1;

  for n = 2 to 25 do
    assert Log2(2 ^ n) == n;
  end;

  return 0;
end;
