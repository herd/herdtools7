func main () => integer
begin
    // Illegal: the bitvector width and the mask
    // length must be equal.
    assert '101010' IN {'xx10101'};

    // Illegal: the length of '1' and '' are different.
    let match_empty_mask_false = '1' IN {''};

    return 0;
end;
