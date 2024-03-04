type MyType of integer;

func foo (x: MyType) => MyType
begin
  return x;
end

func main () => integer
begin
  var x: MyType;

  x = 4;
  x = foo (x as MyType);
  
  let y: MyType = x;

  assert x as MyType == x;

  return 0;
end

