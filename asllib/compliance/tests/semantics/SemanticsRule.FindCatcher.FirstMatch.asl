type ExceptionType1 of exception{code: integer};
type ExceptionType2 of exception{code: integer};

func main() => integer
begin
    var caught = FALSE;
    try
        throw ExceptionType2{code=42};
    catch
        when ExceptionType1 =>
            assert FALSE;
        when e: ExceptionType2 =>
            assert e.code == 42;
            caught = TRUE;
        when ExceptionType2 =>
            assert FALSE;
        otherwise =>
            assert FALSE;
    end;
    assert caught;
    return 0;
end;
