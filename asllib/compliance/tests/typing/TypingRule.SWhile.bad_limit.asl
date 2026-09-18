func scan{N}(x: bits(N)) => integer{0..N}
begin
    var res : integer = 0;
    var i: integer = 0;
    var i_limit : integer = 1000; // Unconstrained integer.
    // Unconstrained integer expressions cannot be used
    // as limit expressions.
    while i < N looplimit i_limit do
        if x[i] == '1' then
            res = res + 1;
        end;
        i = i + 1;
    end;
    return res as integer{0..N};
end;
