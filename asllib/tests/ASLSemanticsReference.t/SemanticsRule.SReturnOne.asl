func f () => integer
begin
  var x : integer = 0;
  for i = 0 to 5 do
    x = x + 1;
    assert x == 1; // Only the first loop is executed
    return 3;
  end
end

func main () => integer
begin

  assert f () == 3;

  return 0;
end

