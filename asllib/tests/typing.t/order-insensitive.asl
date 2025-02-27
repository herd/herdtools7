func foo{N, M}(bv_n: bits(N), bv_m: bits(M))
begin
    let - : integer{N, M} = if ARBITRARY: boolean then N else M;
    let - : integer{M, N} = if ARBITRARY: boolean then N else M;
    let - : integer{M, N} = N;
    let - : integer{M, N} = ARBITRARY: integer {N, M};
    let - : integer{M, N} = ARBITRARY: integer {M, N};
    let - : integer{M, 10, N} = ARBITRARY: integer {M, N};
end;
