func approx_binop_over_constraints{M: integer{4,8}, N: integer{4,8}}(
    x: bits(M),
    y: bits(N + N)) => boolean
begin
    return ((x :: x) as bits(2 * N)) == y;
end;
