func main () => integer
begin
    // Illegal: wrong order of tuple elements, per-position types don't match.
    assert (3, '101010') IN { ('xx1010', 5) };
    return 0;
end;
