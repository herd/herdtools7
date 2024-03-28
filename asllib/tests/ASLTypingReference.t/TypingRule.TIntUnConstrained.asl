type MyType of integer; 
func foo (x: integer) => integer
begin
  return x;
end

func main () => integer
begin
  var x: integer;

  x = 4;
  x = foo (x as integer);
  
  let y: integer = x;

  assert x as integer == x;

  return 0;
end

