func f{N}(a: bits(N), i: integer {1..N -1})
begin
    // a is an under-constrained bitvector of width `N`
    let b: bits(N) = a[i -1:0] :: // bitvector of width (i)
        a[N -1:i] // bitvector of width (N-i)
    ; // result is constrained of width N
end;

func g{P: integer {2,4,8}}() => bits(P)
begin
    var opA = Zeros{P-1}() :: '1';
    return opA;
end;
