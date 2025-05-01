func foo{N, M}(bv_n: bits(N), bv_m: bits(M))
begin
    let a : integer{N, M} = if ARBITRARY: boolean then N else M;
    let b : integer{M, N} = if ARBITRARY: boolean then N else M;
    let c : integer{M, N} = N;
    let d : integer{M, N} = ARBITRARY: integer {N, M};
    let e : integer{M, N} = ARBITRARY: integer {M, N};
    let f : integer{M, 10, N} = ARBITRARY: integer {M, N};
end;
