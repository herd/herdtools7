var X: integer = 0;
config Y = X + 3;

func main () => integer
begin
  assert (Y == 3);

  return 0;
end;

