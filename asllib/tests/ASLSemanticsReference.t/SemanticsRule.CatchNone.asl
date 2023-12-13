     try throw
       MyExceptionType1 {};
       print("No exception raised");
       catch
         when MyExceptionType2 =>
         print("MyException2");
