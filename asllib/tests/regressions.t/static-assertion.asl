pure func foo(x: integer{2,3}) => integer{2,3}
begin
  assert FALSE;
  return x;
end;

constant y = 2 as integer{foo(2)};
