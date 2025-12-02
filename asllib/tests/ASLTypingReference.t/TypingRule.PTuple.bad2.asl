func main () => integer
begin
    // Illegal: both discriminant expression and tuple patterns
    // must have the same length.
    assert 3 IN { (3, 4) };
    return 0;
end;
