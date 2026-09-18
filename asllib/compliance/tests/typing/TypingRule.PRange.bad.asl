func main () => integer
begin
    // Illegal: both expressions in the range patterns must be real-typed.
    assert 42.4 IN { -1.8..143 };
    // Illegal: both expressions in the range patterns must be integer-typed.
    assert 42 IN { -1.8..143 };
    var x : integer = 42;
    // Illegal: pattern expressions must be symbolically evaluable.
    assert 42 IN { -6..x };
    return 0;
end;
