func positive9(N : integer {8,16}, M : integer {8,16})
begin
    let testF : bits(N)   = Zeros(N DIV 2) :: Zeros(N DIV 2); // type system must work out that [bits(N/2), bits(N/2)] is the same as bits(N)
end
