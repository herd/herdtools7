func main () => integer
begin
    // Illegal: both discriminant expression and tuple patterns
    // must have the same length.
    //assert 3 IN { (3, 4) };

    // Illegal: wrong order of tuple elements, per-position types don't match.
    assert (3, '101010') IN { ('xx1010', 5) };
    return 0;
end;
