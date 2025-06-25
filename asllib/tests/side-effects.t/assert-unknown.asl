readonly func unknown () => integer {0, 1}
begin
  return ARBITRARY: integer {0, 1};
end;

func main () => integer
begin
  assert unknown () == 0;

  return 0;
end;
