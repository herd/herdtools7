// a may be initialized from b
func positive3{N}(bv : bits(N))
begin
    var b = 0; // b has type integer{0}
    var a : integer {0..N} = b;
end;
