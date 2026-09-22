func widCheck{N, M}(b: bits(M)) => bits(N)
begin
    if (N == M) then
        // b has the type bits(M), but we know from the previous line that N==M
        // so it is safe to assert that b is within the domain of bits(N).
        // The resulting type of the asserting type conversion expression is bits(N),
        // matching the required return type.
        return b as bits(N);
    else
        return Zeros{N};
    end;
end;
