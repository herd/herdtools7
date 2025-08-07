func foo(x: integer)
begin
  println x;
  if x <= 0 then return; end;
  foo(x - 1);
end;
