func main() => integer
begin
    try
        could_throw_exception();
    catch
        when Excp =>
            handle_excp();
        when exn: Excp2 =>
            handle_excp2(exn);
            throw exn;
        otherwise =>
            unhandled();
    end;
    return 0;
end;

type Excp of exception{-};
type Excp2 of exception{-};

func could_throw_exception()
begin
    if ARBITRARY: boolean then
        throw Excp{-};
    end;
end;

func handle_excp()
begin pass; end;

func handle_excp2(exn: Excp2)
begin pass; end;

func unhandled()
begin pass; end;
