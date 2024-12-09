func foo () => integer
begin
  var x: integer = 0;

  return x;
end;

config Y = foo ();

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
