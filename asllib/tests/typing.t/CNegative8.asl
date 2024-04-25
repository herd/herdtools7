// can't assign integer with (potentially) different constraints to another
func negative8{N, M}(bv : bits(N), bv2 : bits(M))
begin
    var a : integer{0..N};
    var b : integer{0..M};

    a = b; // illegal, would require ATC
end
