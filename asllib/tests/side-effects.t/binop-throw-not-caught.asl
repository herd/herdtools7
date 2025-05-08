type E of exception {-};
type F of exception {-};

func throws_E () => integer
begin
  throw E {-};
end;

func caught_F () => integer
begin
  try
    return throws_E ();
  catch
    when F => return 0;
  end;
end;

func main () => integer
begin
  try
    let x = throws_E () + caught_F ();
  catch
    when E => print ("E caught");
    when F => print ("F caught");
  end;
  
  return 0;
end;
