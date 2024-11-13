func main () => integer
begin
  let x = (2, 3);
  assert x.item0 == 2;
  assert x.item1 == 3;

  return 0;
end;
