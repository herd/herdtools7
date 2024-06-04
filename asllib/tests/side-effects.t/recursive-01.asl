type E of exception {};

func f (x: integer) => integer
begin
  if x >= 0 then
    return 1 + g (x);
  else
    return 0;
  end
end

func g (x: integer) => integer
begin
  if x < 0 then
    throw E {};
  else
    return f ( x - 1 );
  end
end

func main () => integer
begin
  let -  = (f (3), f (4));
  return 0;
end

