type MyType of real; // An alias of real

func circle_circumference(radius: real) => real
begin
  let pi = 3.141592;
  return 2.0 * pi * radius;
end;

func main() => integer
begin
  var x: real = Real(5);
  x = circle_circumference(x as real);
  assert x as real == x;
  let y: integer = RoundDown(x);
  return 0;
end;
