type myexception of exception{-};

var b : boolean;

noreturn func Foo()
begin
  while b looplimit 10 do
    throw myexception{-};
  end;
  // implicit `return` here
end;
