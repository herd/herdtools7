// Can use a parameterized integer as a bitvector width in a subprogram body
func positive9{N}(x: bits(N)) => bits(N + N DIV 2)
begin
    let y: bits(N) = Zeros(N);
    let z: bits(N DIV 2) = Zeros(N DIV 2);
    return y :: z;
end
