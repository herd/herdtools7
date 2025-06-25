pure func foo () => integer
begin
  let x: integer = 0;

  return x;
end;

config Y: integer = foo ();

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
