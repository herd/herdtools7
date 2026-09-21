func main () => integer
begin

  let x = '10' :: '11';
  assert x=='1011';

  let y = '' :: '';
  assert y == '';

  return 0;
end;
