type MyType of string;

func foo (x: string) => string
begin
  return x;
end

func main () => integer
begin
  var x: string;

  x = "foo";
  x = foo (x as string);
  
  let y: string = x;

  assert x as string == x;

  return 0;
end
