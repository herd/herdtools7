type MyException of exception{-};

func ThrowingProc()
begin
    // A throw statement.
    throw MyException{-};
end;

func main () => integer
begin
    // Examples of declarations statements.
    let int_immutable: integer {1..1000} = 42;
    var int_mutable: integer = 7;
    var bv = Zeros{64};

    // An assignment statement.
    int_mutable = 9;
    // A tuple assignment statement.
    (int_mutable, bv) = (10, Ones{64});
    // An assertion statement.
    assert bv == Ones{64};

    try
        // A call statement.
        ThrowingProc();
    catch
        when exn: MyException => pass;
        otherwise =>
            // A printing statement.
            println "an unexpected case";
            // An unreachable statement.
            unreachable;
    end;

    // A while loop statement.
    while int_mutable > 0 looplimit 100 do
        int_mutable = int_mutable - 1;
    end;

    // A return statement.
    return 0;
end;
