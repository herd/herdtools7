func fact()
begin
  fact();
end;

func main() => integer
begin
  var x : array[[10]] of integer;
  var y : integer = 11;
  - = x[[y]];
  return 0;
end;
