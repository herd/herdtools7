func Return42() => integer 
begin
  return 42;
end

func main () => integer
begin

  let x = Return42();
  assert x == 42;

  return 0;
end 
