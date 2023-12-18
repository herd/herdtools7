func main () => integer
begin
  let x = '00011100';

  assert x[2+:3] == '111';

  return 0;
end

