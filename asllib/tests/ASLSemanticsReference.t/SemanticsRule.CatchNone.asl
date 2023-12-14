type MyExceptionType1 of exception{};
type MyExceptionType2 of exception{};

func main () => integer
begin

  try
     try 
       throw MyExceptionType1 {};
       assert FALSE;
     catch
       when MyExceptionType2 =>
         assert FALSE;
     end
  catch MyExceptionType1;
    assert TRUE; 
  end

  return 0;
end
