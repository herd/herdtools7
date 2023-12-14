type MyExceptionType of exception{};

func main () => integer
begin

    try 
      throw MyExceptionType {};
      assert FALSE;
    catch
      when MyExceptionType =>
        assert TRUE;
      otherwise =>
        assert FALSE;
    end

  return 0;
end
