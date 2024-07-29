func main () => integer
begin

  var x = '11111111';
  x[3:0] = '0000';
  assert x == '11110000';

  return 0;
end
