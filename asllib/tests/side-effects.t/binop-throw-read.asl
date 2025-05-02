type E of exception {-};
var X: integer = 0;

func throwing () => integer
begin
  throw E {-};
end;

func main () => integer
begin
  try
    let y = throwing () + X;
  catch
    when E => print ("E caught");
  end;

  return 0;
end;
