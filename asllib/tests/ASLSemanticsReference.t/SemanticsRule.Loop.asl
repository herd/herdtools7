func main () => integer
begin

  var i: integer = 0;

  while i <= 3 do
    assert i <= 3;
    i = i + 1;
  end

  return 0;
end
