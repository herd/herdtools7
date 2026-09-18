func check{M: integer{4,8}, N: integer{4,8}}(
    flag: boolean,
    x: bits(M),
    y: bits(N)) => boolean
begin
    if flag then
        return (x as bits(N)) == y; // Legal
    else
        return x == y; // Illegal: M and N are not necessarily equal.
    end;
end;
