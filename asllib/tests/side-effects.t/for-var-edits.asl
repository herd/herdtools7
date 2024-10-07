func main () => integer
begin
  var x: integer = 10;
  var y: integer = 0;

  for i = 0 to x do
    x = y * y + x;
  end;

  return 0;
end;

