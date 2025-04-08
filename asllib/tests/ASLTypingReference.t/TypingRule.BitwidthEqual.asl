func foo{N}(bv: bits(N))
begin
    var x : bits(N DIV 2 + 1) = bv[N DIV 2:0];
    var y : bits(N DIV 2 + N DIV 2) = bv;
    var - : bits(3 * N + N) = Ones{4 * N};
    // The following statement in comment is illegal, since the current equivalence
    // test cannot determine that `N*N` is equal to `N^2`.
    // var - : bits(N^2) = Ones{N * N};
end;

constant FOUR = 4;

func main() => integer
begin
    var bv: bits(2^FOUR) = Zeros{FOUR*FOUR};
    return 0;
end;
