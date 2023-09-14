func Return42() => integer 
begin
  return 42;
end

func main () => integer
begin
  let x = if UNKNOWN: boolean then 3 else Return42();
  assert(x==3);
  return 0;
end 
