type E of exception {};

func throws_E () => integer
begin
  throw E {};
end;

func main () => integer
begin
  try
    let x = throws_E () + UNKNOWN: integer {0..3};
  catch
    when E => print ("Caught E.");
  end;

  return 0;
end;
