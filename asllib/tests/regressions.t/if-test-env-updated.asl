var myglob : integer = 0;
func mytest () => boolean
begin
  myglob = 1;
  return TRUE;
end;
func main () => integer
begin
  var test = if mytest() then 0 else 1;
  assert myglob == 1;
  return 0;
end;
