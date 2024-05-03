// only static types are considered during type-checking assignments
func negative10{N, M}(bv : bits(N), bv2 : bits(M))
begin
    var a : integer{0..N};
    var b : integer{0..M};
    if N == M then
        a = b; // illegal; only the static type is considered for type-checking
    end
end
