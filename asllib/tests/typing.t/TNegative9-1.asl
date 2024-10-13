func negative9(N : integer {8,16}, M : integer {8,16}, X : integer)
begin
    let testB : bits(N) = Zeros(N DIV 4) :: Zeros(N DIV 2); // bits(3N/4) != bits(N)
    // Type of Zeros(N) its bits(N), not bits(M), so this is illegal regardless of the fact that N and M have the same domain,
    // they could have different runtime values so we must evaluate the type safety symbolically
end
