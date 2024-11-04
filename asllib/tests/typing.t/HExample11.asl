func min_highest_set_bit_example{N}(curr : bits(N))
begin
    var highest = HighestSetBit(curr) as integer{0..7};
    var minimum = Min(highest, 7) as integer{0..7};

    let size = minimum;
    var x = Zeros {N};
end;
