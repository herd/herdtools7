type MyType of array [4] of integer;

func foo (x: array [4] of integer) => array [4] of integer
begin
  var y = x;
  y[3] = 2;
  return y;
end

func main () => integer
begin
  var x: array [4] of integer;

  x[1] = 1;
  print(x);
  x = foo (x as array [4] of integer);
  
  let y: array [4] of integer = x;


  return 0;
end
