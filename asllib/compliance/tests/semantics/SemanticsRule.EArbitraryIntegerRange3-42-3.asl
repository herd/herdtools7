func main () => integer
begin

  let x = ARBITRARY:integer {3, 42};
  assert x==3;

  return 0;
end; 
