type MyExceptionType of exception{-};

func main () => integer
begin

    try
      assert TRUE;
    catch
      when MyExceptionType =>
        assert FALSE;
      otherwise =>
        assert FALSE;
    end;
    println("No exception raised");

  return 0;
end;
