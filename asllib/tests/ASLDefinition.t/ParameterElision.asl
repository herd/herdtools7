func X{N}(bv: bits(N), x: integer{0..N-1}) => bits(N)
begin
    var result = Zeros{N};
    result[x] = bv[x];
    return result;
end;

func main() => integer
begin
    var data: bits(64) = ARBITRARY: bits(64);
    var n = ARBITRARY : integer{0..63};
    // The parameter 64 can be elided as it is inferred from LHS
    // via desugaring. This always requires curly braces,
    // even if they are empty, as in the example below.
    var foo : bits(64) = X{}(data, n);

    var result : bits(64);
    // The parameter N is inferred for the standard library function
    // `LSL` based on the first argument via typechecking.
    // In this case the empty {} must be omitted.
    result = LSL(foo, 3);
    return 0;
end;
