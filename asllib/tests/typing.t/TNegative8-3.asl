func negative8()
begin
    let N : integer = 7;
    for i = 31 downto N do
        let testD : integer {7..31} = i; // N is an unconstrained integer, so i is also unconstrained
    end
end
