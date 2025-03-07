func main () => integer
begin
    // Illegal: all patterns in the list must match the type
    // of the pattern discriminant expression.
    assert TRUE IN {FALSE, 5};
    return 0;
end;
