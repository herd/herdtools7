// Assigning to a wider range
func positive7{N}(bv: bits(N), x: integer{0..N})
begin
    var a: integer{0..2*N} = x;
    var b: integer{0..N+1} = x;
end;
