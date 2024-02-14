func f (x: integer) => integer
begin
  if x >= 0 then
    return 1 + g (x - 1);
  else
    return 0;
  end
end

func g (x: integer) => integer
begin
  return f (x);
end

func main () => integer
begin
  assert f(0) == 1;
  for i = -1 to 20 do
    assert f(i) == i + 1;
  end

  return 0;
end
  

