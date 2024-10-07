func main () => integer
begin
  var x: integer = 10;
  var y: integer = 10;

  while (x < y) looplimit 100 do
    x = y * y + x;
  end;

  return 0;
end;


