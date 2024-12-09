func main () => integer
begin
  var x: integer = 10;
  var y: integer = 10;

  repeat
    x = y * y + x;
  until (x > y) looplimit 100;

  return 0;
end;



