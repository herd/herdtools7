func main () => integer
begin
    // Illegal: the bitvectors must have the same length.
    assert '101' IN { '1100' };
    var x = '1101';
    // Illegal: pattern expressions must be symbolically evaluable.
    assert '101' IN { x };
    return 0;
end;
