func myfunction(x: boolean) => integer
begin
    if x == TRUE then
        return 1;
    else
        doesnotreturn();
    end;
end;

type myexception of exception;

noreturn func doesnotreturn()
begin
    throw myexception{-};
end;

func main() => integer
begin
    return 1;
end;
