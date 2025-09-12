type myexception of exception{-};

var b : boolean;

noreturn func Foo()
begin
  repeat
    throw myexception{-};
  until b looplimit 10;
  // implicit `return` here
end;
