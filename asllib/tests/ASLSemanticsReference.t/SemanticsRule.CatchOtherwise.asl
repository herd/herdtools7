     try throw
       MyExceptionType1 {};
       print("No exception raised");
       catch
         when MyExceptionType2 =>
         print("MyException2");
       otherwise =>
         print("Another exception");
