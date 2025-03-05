constant A = 1 << 10;
constant B = 1 << 20;

func myfunction(a: integer {0..A}, b: integer {1..B})
begin
  var z : integer {-} = a * b;
end;

func main () => integer
begin
  myfunction(A, 1);
  myfunction(0, 1);

  return 0;
end;
