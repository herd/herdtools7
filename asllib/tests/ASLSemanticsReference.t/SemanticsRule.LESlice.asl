func main () => integer
begin
  var x = '11 11 1111';
  x[3:0, 7:6] = '000000';
  assert x == '00110000';

  x[(1)*:(2)] = Ones{2};
  assert x == '00111100';

  return 0;
end;
