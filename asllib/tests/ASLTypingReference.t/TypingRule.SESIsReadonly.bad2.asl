var g : integer = 0;

func write_side_effecting() => integer
begin
    g = 2;
    return g;
end;

func main() => integer
begin
    var x = 15;
    var y = x + 9;

    // The following statement is illegal, since
    // the expression `write_side_effecting()` is not readonly.
    for i = x to write_side_effecting() do
        g = g + 1;
    end;
    return 0;
end;
