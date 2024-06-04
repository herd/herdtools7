var X: integer = 0;
var Y: integer = 0;

func WriteX() => integer
begin
  let t = X;
  X = t + 1;
  return t;
end

func WriteY() => integer
begin
  let t = Y;
  Y = t + 1;
  return t;
end

func main () => integer
begin
  let - = (WriteX(), WriteY());
  return 0;
end

