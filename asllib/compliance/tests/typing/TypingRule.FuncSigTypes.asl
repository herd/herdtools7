// The list of signature types is: integer{0..N}, real, bits(N).
func proc{N}(x: integer{0..N}, y: real, z: bits(N))
begin
    pass;
end;

// The list of signature types is: bits(N), integer{0..N}, real.
func returns_value{N}(x: integer{0..N}, y: real) => bits(N)
begin
    return Zeros{N};
end;
