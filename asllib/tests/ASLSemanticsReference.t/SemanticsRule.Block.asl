func main() => integer
begin
  var x : integer = 1;

  if TRUE then x = 2; let y = 2;  else pass; end
  let y = 1;
  assert (x == 2 && y == 1);

  return 0;
end
