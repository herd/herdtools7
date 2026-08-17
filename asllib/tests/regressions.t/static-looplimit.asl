pure func foo(x: integer{2,3}) => integer{2,3}
begin
  for i = 0 to 0 looplimit 0 do
    pass;
  end;
  return x;
end;

constant y = 2 as integer{foo(2)};
