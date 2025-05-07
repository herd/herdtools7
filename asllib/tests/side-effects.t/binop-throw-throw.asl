type E of exception {-};

func throwing () => integer
begin
  throw E {-};
end;

func main () => integer
begin
  try
    let y = throwing () + throwing ();
  catch
    when E => print ("E caught");
  end;

  return 0;
end;

