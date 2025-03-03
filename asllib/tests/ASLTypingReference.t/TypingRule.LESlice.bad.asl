func main () => integer
begin
  var x = '11 11 1111';
  x[3:0, 3] = '0 0000';
  assert x == '11110000';
  return 0;
end;
