var X: integer = 0;
var Y: integer = 0;

func WriteX() => integer
begin
  let t = X;
  X = t + 1;
  return t;
end

func ReadY() => integer
begin
  return Y;
end

func main () => integer
begin
  let - = (WriteX(), ReadY());
  return 0;
end

