func approx_max{N}(d: integer{N})
begin
    let a: integer{1..5} = ARBITRARY: integer{1..5};
    let b: integer{a..2*a, 1} = ARBITRARY: integer{a..2*a};
    let c: integer{a..2*N, 1} = ARBITRARY: integer{a..2*N};
end;
