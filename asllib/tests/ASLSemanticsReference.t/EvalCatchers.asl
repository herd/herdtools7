type MyExceptionType of exception{-};
var g : integer = 0;

func update_and_throw()
begin
    var x = 5;
    g = 1;
    throw MyExceptionType{-};
end;

func main() => integer
begin
    var x = 2;
     try
       update_and_throw();
     catch
       when MyExceptionType =>
         println(x, g);
     end;
  return 0;
end;
