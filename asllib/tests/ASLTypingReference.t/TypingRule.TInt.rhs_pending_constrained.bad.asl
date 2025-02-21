func main() => integer
begin
    // Pending-constrained integer types are illegal
    // in right-hand-side expressions.
    var x : integer{1..2} = 3 as integer{-};
    return 0;
end;
