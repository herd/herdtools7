pure func foo(x: integer{2,3}) => integer{2,3}
begin
  var arr : array[[-1]] of integer;
  return x;
end;

constant y = 2 as integer{foo(2)};
