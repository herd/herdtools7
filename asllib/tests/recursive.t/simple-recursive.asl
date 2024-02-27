func f (x: integer) => integer
begin
  if x >= 0 then
    return 1 + f (x - 1);
  else
    return 0;
  end
end

func main () => integer
begin
  assert f(0) == 1;
  for i = -1 to 20 do
    assert f(i) == i + 1;
  end

  return 0;
end
  

