func main () => integer
begin
  assert 42 IN { >= 3 };
  assert 42.0 IN { >= 3.0 };
  return 0;
end;
