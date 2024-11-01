func negative8()
begin
    let N : integer = 7;
    for i = N downto 0 do
        let testB : integer {0..7}  = i; // N is an unconstrained integer, so i is also unconstrained
    end;
end;
