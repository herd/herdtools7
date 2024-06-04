type E of exception {};
var X: integer = 0;

func Throws() => integer
begin
  throw E {};
end

func ReadX() => integer
begin
  return X;
end

func main () => integer
begin
  try
    let - = (Throws (), ReadX ());
  catch when E => pass; end

  return 0;
end


