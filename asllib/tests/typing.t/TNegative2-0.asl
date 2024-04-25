func negative2(size : integer {0..3}, size2 : integer {8,16,32,64}, myInt : integer)
begin
    // assignment to a variable with a domain thats a subset is illegal without ATC's
    let testA : integer {0..2}    = size;
end
