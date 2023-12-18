func Return42() => integer 
begin
  return 42;
end

func main () => integer
begin

  let x = if FALSE then Return42() else 3;
  assert x==3;

  return 0;
end 
