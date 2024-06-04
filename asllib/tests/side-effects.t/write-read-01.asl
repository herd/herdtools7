var X: integer = 0;

func WriteX() => integer
begin
  let t = X;
  X = t + 1;
  return t;
end

func ReadX() => integer
begin
  return X;
end

func main () => integer
begin
  let - = (WriteX(), ReadX());
  return 0;
end


