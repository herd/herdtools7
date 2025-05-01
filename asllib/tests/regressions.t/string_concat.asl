func main() => integer
begin
  assert ("foo" :: 1 :: TRUE :: '1') == "foo1TRUE0x1";
  return 0;
end;
