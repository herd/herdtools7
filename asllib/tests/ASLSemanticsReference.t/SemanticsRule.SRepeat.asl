func main () => integer
begin

  var i: integer = 0;
  repeat
    assert i <= 3;
    println(i);
    i = i + 1;
  until i > 3 looplimit 4;

  return 0;
end;
