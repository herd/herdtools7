func main () => integer
begin
  var i: integer = 0;
  repeat
    i = i + 1;
    print(i);
  until (i >= 10) looplimit 5;

  return 0;
end;

