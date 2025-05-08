type E of exception {-};

func foo (x: integer) => integer
begin
  throw E {-};
end;

constant C = foo (4);

func main () => integer
begin
  assert C == 19;

  return 0;
end;

