func fail() => boolean
begin
  assert FALSE;
  return TRUE;
end

func main () => integer
begin
  let b = FALSE && fail();
  assert b == FALSE;
  return 0;
end 
