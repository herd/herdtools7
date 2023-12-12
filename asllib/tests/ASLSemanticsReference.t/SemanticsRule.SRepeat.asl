func main () => integer
begin

  var i: integer = 0;
  repeat
    assert i <= 3;
    i = i + 1;
  until i > 3;

  return 0;
end
