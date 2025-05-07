type MyException of exception;

func throwing_func()
begin
    throw MyException{-};
end;

func non_throwing_func()
begin
    pass;
end;

func main() => integer
begin
    non_throwing_func();
    try
        throwing_func();
    catch
        when MyException => pass;
    end;
    return 0;
end;
