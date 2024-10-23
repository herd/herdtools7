func unknown () => integer {0..10}
begin
  return UNKNOWN: integer {0..10};
end;

func main () => integer
begin
  for i = 0 to unknown () do
    pass;
  end;

  return 0;
end;

