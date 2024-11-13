func negative8()
begin
    let N : integer = 7;
    for i = 0 to N do
        let testA : integer {0..7}  = i; // N is an unconstrained integer, so i is also unconstrained
    end;
end;
