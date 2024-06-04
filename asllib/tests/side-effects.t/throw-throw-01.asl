type E of exception {};

func Throws() => integer
begin
  throw E {};
end

func main () => integer
begin
  try
    let - = (Throws (), Throws ());
  catch when E => pass; end

  return 0;
end


