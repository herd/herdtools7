func main () => integer
begin
    // Illegal: the bitvector width and the mask
    // length must be equal.
    assert '101010' IN {'xx10101'};
    return 0;
end;
