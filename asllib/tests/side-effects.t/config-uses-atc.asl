func foo () => integer
begin
  return ARBITRARY: integer {0..10};
end;

config Y = foo ();

func main () => integer
begin
  assert (Y == 0);

  return 0;
end;
