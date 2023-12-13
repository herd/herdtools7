    try throw
      MyExceptionType { msg: "My exception with my message" };
    catch
      when MyExceptionType: exn =>
      print(exn.msg);
    otherwise =>
    print("Another exception");
