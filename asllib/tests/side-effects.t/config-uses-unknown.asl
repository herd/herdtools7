func foo () => integer
begin
  return ARBITRARY: integer {0..10};
end;

config Y = foo ();

func main () => integer
begin
  assert Y <= 10;

  return 0;
end;
