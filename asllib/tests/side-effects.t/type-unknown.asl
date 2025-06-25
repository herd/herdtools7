pure func unknown () => integer {0, 1}
begin
  return ARBITRARY: integer {0, 1};
end;

func main () => integer
begin
  let x = 0 as integer{unknown ()};
  let y: integer {unknown ()} = x;

  return 0;
end;

