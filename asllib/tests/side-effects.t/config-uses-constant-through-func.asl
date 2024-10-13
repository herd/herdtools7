constant X: integer = 0;

func foo () => integer
begin
  return X;
end;

config Y = foo ();

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
