    try throw
      MyExceptionType {};
      print("No exception raised");
    catch
      when MyExceptionType =>
      print("MyException");
    otherwise =>
      print("Another exception");
