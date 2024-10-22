func positive9(N : integer {8,16}, M : integer {8,16})
begin
    let testA : bits(8)   = Zeros(8);
    let testB : bits(8)   = Zeros(4+4);
    let testC : bits(8)   = Zeros(4) :: Zeros(4);
    let testD : bits(N)   = Zeros(N);   // type of Zeros(N) its bits(N)
    let testE : bits(N-1) = Zeros(N-1); // type of Zeros(N-1) its bits(N-1)
    let testF : bits(N)   = Zeros(N DIV 2) :: Zeros(N DIV 2); // type system must work out that [bits(N/2), bits(N/2)] is the same as bits(N)
    let testG : bits(M)   = Zeros(N) as bits(M);
    for i = 0 to 7 do
        let testH : bits(i) = Zeros(i); // i is both immutable and a constrained integer, so can be used as a bit width
    end
end
