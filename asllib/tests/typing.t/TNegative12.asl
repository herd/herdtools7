func negative12(N : integer {8,16})
begin
    let testA = N     as bits(8); // ATC's can't change structure.
    let testB = testA as integer;
end;

