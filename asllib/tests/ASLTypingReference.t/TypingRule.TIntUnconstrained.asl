type MyType of integer;
func foo (x: integer) => integer
begin
  return x;
end;

func main () => integer
begin
  var x: integer;

  x = 4;
  x = (x + foo (x as integer)) - 1000;

  let z: integer = 5;
  let w = foo(z);
  let y: integer = x * z;

  assert x as integer == x;

  return 0;
end;
