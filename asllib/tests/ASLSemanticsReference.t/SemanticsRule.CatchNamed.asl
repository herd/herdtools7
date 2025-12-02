type MyExceptionType of exception{ msg: integer };

func main () => integer
begin

    try
      throw MyExceptionType { msg=42 };
    catch
      when exn: MyExceptionType =>
        println "caught MyExceptionType with msg=", exn.msg;
        assert exn.msg == 42;
    otherwise =>
      assert FALSE;
    end;

  return 0;
end;
