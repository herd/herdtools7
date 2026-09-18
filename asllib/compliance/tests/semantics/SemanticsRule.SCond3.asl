func UNPREDICTABLE ()
begin
  assert FALSE;
end;

func main () => integer
begin
  var d: integer = ARBITRARY : integer{13, 16};
  var n: integer = d - 1;

  if d IN {13,15} || n IN {13,15} then
      UNPREDICTABLE();
  end;

  return 0;
end;
