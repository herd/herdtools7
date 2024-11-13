getter f1 => bits(4)
begin
  return '0000';
end;

setter f1 = v: bits(4)
begin
  pass;
end;

func main () => integer
begin
  f1[] = '';
  
  return 0;
end;
