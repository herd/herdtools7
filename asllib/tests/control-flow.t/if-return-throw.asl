type E of exception {};

func foo (x: integer) => integer
begin
  if x >= 0 then return x;
  else throw E {};
  end
end

func main () => integer
begin
  assert foo (2) == 2;
  assert foo (3) == 3;

  return 0;
end

