func foo(x: integer) recurselimit 3
begin
  println x;
  if x <= 0 then return; end;
  foo(x - 1);
end;

func main() => integer
begin
  foo(2);
  return 0;
end;
