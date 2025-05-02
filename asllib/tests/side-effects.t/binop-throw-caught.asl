type E of exception {-};

func throws_E () => integer
begin
  throw E {-};
end;

func caught_E () => integer
begin
  try
    return throws_E ();
  catch
    when E => return 0;
  end;
end;

func main () => integer
begin
  try
    let x = throws_E () + caught_E ();
  catch
    when E => print ("E caught");
  end;

  return 0;
end;
