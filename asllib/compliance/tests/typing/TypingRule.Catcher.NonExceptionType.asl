type IntegerType of integer;

func main() => integer
begin
    try
        pass;
    catch
        when caught: IntegerType =>
            pass;
    end;
    return 0;
end;
