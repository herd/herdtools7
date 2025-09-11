constant FIVE = 5;
constant SEVEN = 7;
var g : integer = 3;
let g2 = 2^SEVEN;

func add_3(x: integer) => integer
begin
    return x + 3;
end;

type MyRecord of record { data: bits(8) };

constant error_msg = "error";
type MyException of exception { msg: string };

func procedure()
begin
    return; // { }
end;

func sequence_stmt()
begin
    // The identifiers use by the following sequence of statements
    // are { Other(FIVE), Other(SEVEN) }
    g = FIVE;
    g = SEVEN;
end;

func return_val(x: integer) => integer
begin
    return x + g; // { Other(g), Other(x) }
    unreachable;  // { }
end;

func throw_stmt()
begin
    throw MyException{ msg=error_msg }; // { Other(MyException), Other(error_msg) }
end;

func main() => integer
begin
    pass; // { }
    assert FIVE != SEVEN; // { Other(FIVE), Other(SEVEN) }
    g = FIVE; // { Other(g), Other(SEVEN) }
    - = return_val(FIVE); // { Subprogram(return_val), Other(FIVE) }
    if g == SEVEN then // { Other(g), Other(SEVEN), Other(FIVE) }
        pass;
    else
        g = FIVE;
    end;
    for i = FIVE to SEVEN looplimit 2^g2 do // { Other(FIVE), Other(SEVEN), Other(g2) }
        pass;
    end;
    var y : integer = g2; // { Other(g2) }
    try // { Other(g), Other(g2), Other(MyException) }
        y = g;
        throw_stmt();
    catch
        when MyException => // { Other(MyException), Other(g2) }
            y = g2;
            println "caught MyExeption";
    end;
    println y; // { Other(y) }
    return 0;
end;
