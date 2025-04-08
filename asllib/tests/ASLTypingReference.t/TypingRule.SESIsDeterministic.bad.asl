var g : integer = 0;

func main() => integer
begin
    var x = 15;
    var y = x + 9;

    // The following statement is illegal, since
    // the expression `ARBITRARY : integer{1..1000}` is not deterministic.
    for i = x to ARBITRARY : integer{1..1000} do
        g = g + 1;
    end;
    return 0;
end;
