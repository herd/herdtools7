func main () => integer
begin
  var x: integer = 10;
  var y: integer = 0;

  repeat
    x = y * y + x;
  until (x > y);

  return 0;
end



