func myfunction(x: boolean) => integer
begin
    if x == TRUE then
        return 1;
    else
        doesnotreturn();
    end;
end;

type myexception of exception{-};

noreturn func doesnotreturn()
begin
    alsodoesnotreturn();
end;

noreturn func alsodoesnotreturn()
begin
    finaldoesnotreturn();
end;

noreturn func finaldoesnotreturn()
begin
    if ARBITRARY: boolean then
        throw myexception{-};
    else
        unreachable;
    end;
end;

noreturn func rec_noreturning()
begin
    rec_noreturning();
end;

func main() => integer
begin
    return 1;
end;
