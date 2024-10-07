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
  let f0 = f(0);
  assert f0 == 1;
  for i = -1 to 20 do
    let fi = f(i);
    assert fi == i + 1;
  end

  return 0;
end
  

