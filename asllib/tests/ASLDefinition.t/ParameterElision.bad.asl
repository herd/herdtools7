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
    // Illegal: must specify an empty list of parameters, {}.
    var foo : bits(64) = X(data, n);
    return 0;
end;
