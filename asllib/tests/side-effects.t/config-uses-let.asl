let X: integer = 0;
config Y = X;

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
