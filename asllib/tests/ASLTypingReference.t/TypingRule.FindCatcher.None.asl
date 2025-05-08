type ExceptionType1 of exception{-};
type ExceptionType2 of exception{ msg: string};
type ExceptionType3 of exception{ msg: string};
var g : integer = 0;

func update_and_throw()
begin
    var x = 5;
    g = 1;
    throw ExceptionType2{msg="ExceptionType2"};
end;

func main() => integer
begin
    var x = 2;
     try
       update_and_throw();
     catch
        when ExceptionType1 =>
            println("ExceptionType1", " : x=", x, ", g= ", g);
        when named_e: ExceptionType2 =>
            println(named_e.msg, " : x=", x, ", g= ", g);
     end;
  return 0;
end;
