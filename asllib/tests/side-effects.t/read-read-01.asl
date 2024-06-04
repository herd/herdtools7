var X: integer = 0;

func ReadX() => integer
begin
  return X;
end

func main () => integer
begin
  let - = (ReadX(), ReadX());
  return 0;
end


