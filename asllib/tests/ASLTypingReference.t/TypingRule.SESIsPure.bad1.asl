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
    assert y > x;

    // The following statement is illegal, since
    // the expression `write_side_effecting()` is not pure.
    assert y > write_side_effecting();
    return 0;
end;
