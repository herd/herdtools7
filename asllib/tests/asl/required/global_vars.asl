
var x :: integer = 0;

func incr ()
begin
  x = x + 1;
end

func incr2 () => integer
begin
  let y = x;
  x = y + 1;
  return y;
end

func add (x : integer, y: integer) => integer
begin
  return x + y;
end

func main ()
begin
  incr ();
  incr ();
  incr ();
  assert x == 3;

  assert incr2 () == 3;
  assert 2 + incr2 () * 4 == 18;
  // assert add (incr2 (), 3) == 8;
  assert x == 5;

end

