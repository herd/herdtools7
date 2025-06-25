pure func X(a: integer) => integer
begin
  return 0;
end;

func X(b: boolean) // Illegal: differing qualifier to the function `X`
begin
  pass;
end;
