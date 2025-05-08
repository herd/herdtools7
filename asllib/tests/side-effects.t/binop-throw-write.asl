type E of exception {-};
var X: integer = 0;

func throwing () => integer
begin
  throw E {-};
end;

func set_and_return () => integer
begin
  X = 3;
  return 2;
end;

func main () => integer
begin
  try
    let y = throwing () + set_and_return ();
  catch
    when E => print ("E caught");
  end;

  return 0;
end;

