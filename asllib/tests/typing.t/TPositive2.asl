////////////////////////////////////////////////////////////////////////////////
// Assignments of constrained integers
////////////////////////////////////////////////////////////////////////////////
func positive2(size : integer {0..3}, size2 : integer {8,16,32,64})
begin
    // assignment to a variable with a domain that's a superset is valid,
    let testA : integer {0..4}           = size;
    let testB : integer {8,16,32,64,128} = size2;
    // The way the domain is expressed doesn't matter, so {0..3} is the same as {0,1,2,3}
    let testC : integer {0,1,2,3}        = size;
    let testD : integer {0..3}           = testC;
    // Combinations of both of the above are also legal
    let testE : integer {0,1,2,3,4}      = size;
    let testF : integer {0..128}         = size2;
end;

