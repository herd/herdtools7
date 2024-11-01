// a may not be initialized from b
func negative1{N}(bv : bits(N))
begin
    var b : integer {-1} = -1;
    var a : integer {0..N} = b; // illegal
end;
