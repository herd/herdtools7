// M is not guaranteed to be within the constraints of z
func negative11{N, M}(x: bits(N), y: bits(M))
begin
    var z = 0 as integer{0..N};
    z = M; // illegal
end;
