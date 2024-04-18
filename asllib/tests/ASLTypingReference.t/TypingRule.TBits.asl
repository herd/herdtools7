type MyType of bits(4);

func foo (x: bits(4)) => bits(4)
begin
  return NOT x;
end

func main () => integer
begin
  var x: bits(4);

  x = '1010';
  x = foo (x as bits(4));
  
  let y: bits(4) = x;

  assert x as bits(4) == x;

  return 0;
end

