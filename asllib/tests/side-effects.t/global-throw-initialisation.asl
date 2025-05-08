type E of exception {-};

func throwing () => integer
begin
  throw E {-};
end;

let X: integer = throwing ();

func main () => integer
begin
  println (X);

  return 0;
end;
