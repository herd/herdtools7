config X: integer = 0;
config Y: integer = X;

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
