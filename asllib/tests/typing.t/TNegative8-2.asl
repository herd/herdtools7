func negative8()
begin
    let N : integer = 7;
    for i = N to 31 do
        let testC : integer {7..31} = i; // N is an unconstrained integer, so i is also unconstrained
    end
end
