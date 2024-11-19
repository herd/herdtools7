func positive12(N : integer {8,16})
begin
    let testA : bits(8)  = Zeros{N}  as bits(8);
    let testB : bits(N)  = Zeros{8}  as bits(N);
    // ATC's permitted to be disjoint with bit vector they're applied to. Must compile, but will fail at runtime if the line is executed.
    // See ASL-313 for rational.
    let testC : bits(32) = Zeros{N}  as bits(32);
    let testD : bits(N)  = Zeros{32} as bits(N);
    let testE : bits(8)  = Zeros{32} as bits(8);
end;

