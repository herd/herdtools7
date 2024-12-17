var X: integer = 0;

func incr_X () => integer
begin
  let x = X;
  X = x + 1;
  return x;
end;

func main () => integer
begin
  println (incr_X ());
  println (incr_X ());
  println (incr_X ());

  return 0;
end;
