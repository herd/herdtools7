let X: integer = 0;

pure func foo () => integer
begin
  return X;
end;

config Y: integer = foo ();

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
