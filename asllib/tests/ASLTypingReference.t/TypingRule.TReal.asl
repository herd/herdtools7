type MyType of real;

func foo (x: real) => real
begin
  return x + 1.0;
end

func main () => integer
begin
  var x: real;

  x = 3.141592;
  x = foo (x as real);
  
  let y: real = x + x;

  assert x as real == x;

  return 0;
end

