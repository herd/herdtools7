type E of exception {-};

readonly func throwing () => integer
begin
  throw E {-};
end;

func main () => integer
begin
  assert throwing () == 0;

  return 0;
end;


