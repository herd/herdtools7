func negative2(size : integer {0..3}, size2 : integer {8,16,32,64}, myInt : integer)
begin
    // assignment to a variable with a domain thats a subset is illegal without ATC's
    let testC : integer {8,16,32} = myInt; // assignment of unconstrained integers to constrained integers is also illegal without a ATC
end;
