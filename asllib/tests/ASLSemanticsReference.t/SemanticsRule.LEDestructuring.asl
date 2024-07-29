func main () => integer
begin

  var x: integer = 42;
  var y: integer = 3;
  
  (x, y) = (3, 42);

  assert x == 3 && y == 42;

  return 0;
end
