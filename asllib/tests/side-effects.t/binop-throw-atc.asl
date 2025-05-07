type E of exception {-};

func throwing () => integer
begin
  throw E {-};
end;

func performs_atc () => integer
begin
  return (1 as integer {2});
end;

func main () => integer
begin
  try
    let y = throwing () + performs_atc ();
  catch
    when E => print ("E caught");
  end;

  return 0;
end;

