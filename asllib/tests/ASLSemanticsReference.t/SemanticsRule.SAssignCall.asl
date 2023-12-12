func f(x:integer) => (integer, integer)
begin
  return (x,x+1);
end

func main() => integer
begin
  var a,b : integer;

  (a,b) = f(1);

  assert (a+b == 3);
  return 0;
end
