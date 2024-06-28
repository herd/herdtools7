// y has the type integer {0..N}
func positive1{N}(x: bits(N), offset: integer)
begin
    let y = offset MOD N;
    let z: integer {0..N} = y;
end
