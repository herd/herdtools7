func main () => integer
begin
    // Illegal: integers can only be compared to integers.
    assert 42 IN { >= 3.0 };
    // Illegal: reals can only be compared to reals.
    assert 42.0 IN { >= 3 };
    var x : integer;
    // Illegal: expressions must be symbolically evaluable.
    assert 42.0 IN { >= x };
    return 0;
end;
