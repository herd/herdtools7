type E of exception {};
var X: integer = 0;

func Throws() => integer
begin
  throw E {};
end

func WriteX() => integer
begin
  let t = X;
  X = t + 1;
  return t;
end

func main () => integer
begin
  try
    let - = (Throws (), WriteX ());
  catch when E => pass; end
  return 0;
end


