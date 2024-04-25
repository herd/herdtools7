func negative9(N : integer {8,16}, M : integer {8,16}, X : integer)
begin
    let testE : bits(N) = Zeros(8); // N != 8, even though 8 is in the constraint set for N. N could be 16 after all.
end
