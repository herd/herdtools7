func main () => integer
begin
    let bv = ARBITRARY: bits(4);
    let val = ARBITRARY: integer;

    var status: integer;
    // Example of the match-any pattern
    // and bitmask pattern.
    if bv IN {'1xx0', '0xx1'} then
        status = 1;

    // Example of the greater-than pattern
    elsif UInt(bv) IN { >= 1000} then
        status = 2;

    // Example of the negation pattern
    elsif bv IN !{ '1111', '0000' } then
        status = 3;

    // Example of the tuple  pattern
    elsif (bv, val) IN { ('0010', >= 1000) } then
        status = 4;

    // Example of the match-all pattern
    elsif bv IN { - } then
        status = 5;
    end;

    return 0;
end;
