type MyExceptionType1 of exception{};
type MyExceptionType2 of exception{};

func main () => integer
begin

     try 
       throw MyExceptionType1 {};
       assert FALSE;
     catch
       when MyExceptionType2 =>
         assert FALSE;
       otherwise =>
         assert TRUE;
     end

  return 0;
end
