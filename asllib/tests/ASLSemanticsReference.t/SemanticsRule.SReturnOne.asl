func f () => integer
begin
  var x : integer = 0;
  for i = 0 to 5 do
    x = x + 1;
    assert x == 1; // Only the first loop iteration is ever executed
    return 3;
  end;

  assert FALSE;
  return -1;
end;

func main () => integer
begin

  assert f () == 3;

  return 0;
end;

