func main() => integer
begin
  assert (0x12345[:20] == 0x123456789_12345[0+:20]);

  return 0;
end;
