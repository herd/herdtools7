type MyExceptionType of exception{ a: integer };

func main () => integer
begin

  try 
    throw MyExceptionType { a = 42 };
       
  catch 
    when MyExceptionType => assert TRUE;
    otherwise => assert FALSE;
  end

  return 0;
end
