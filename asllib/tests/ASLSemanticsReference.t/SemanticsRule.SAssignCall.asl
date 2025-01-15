func f(x: integer) => (integer, integer, integer)
begin
  return (x, x+1, x+2);
end;

func main() => integer
begin
  var a, b : integer;

  (a, b, -) = f(1);

  assert (a + b == 3);
  return 0;
end;
