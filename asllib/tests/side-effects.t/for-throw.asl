type E of exception {-};

func throwing () => integer
begin
  throw E {-};
end;

func main () => integer
begin
  var x: integer = 10;
  var y: integer = 0;

  for i = 0 to throwing () do
    - = y * y + x ;
  end;

  return 0;
end;

