func Return42() => integer 
begin
  return 42;
end

func main () => integer
begin
  let (x,y) = (3, Return42());
  assert(x==3);
  assert(y==42);
  return 0;
end 
