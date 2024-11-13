func f1()
begin 
  return FALSE;
end;

func checkY (y: integer)
begin
  if (f1() && f2(y as {2,4,8})) then pass; end;
end;
