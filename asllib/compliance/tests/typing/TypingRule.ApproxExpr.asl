func approx_expr_example{M: integer{4,8}, N: integer{4,8}}(
    x: bits(M),
    y: bits(N)) => boolean
begin
    // Checking that bits(M) subtype-satisfies bits(N)
    // involves overapproximating M, yielding {4, 8}, and
    // underapproximating N, yielding {}.
    // Since the {4, 8} is not a subset of {}, the static check
    // fails, and a dynamic check is required.
    return (x as bits(N)) == y;
end;
