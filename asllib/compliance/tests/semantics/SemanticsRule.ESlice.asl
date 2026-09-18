func main () => integer
begin
  let x: bits(5){} = '1 1110 000'[6:3, 7];
  assert x == '1110 1';

  let y: bits(5){} = 240[6:3, 7];
  assert y == x;

  let z: bits(5){} = 496[6:3, 7];
  assert z == x;
  return 0;
end;
