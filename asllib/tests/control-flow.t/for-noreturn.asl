type myexception of exception{-};

noreturn func Foo()
begin
  for i = 1 to 0 do
    throw myexception{-};
  end;
  // implicit `return` here
end;
