var X: integer = 0;

func WriteX() => integer
begin
  let t = X;
  X = t + 1;
  return t;
end

func main () => integer
begin
  let - = (WriteX(), WriteX());
  return 0;
end

