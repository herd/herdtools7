func main() => integer
begin
    var foo : bits(64) = ARBITRARY: bits(64);
    var result : bits(64);
    // The parameter N is inferred for the standard library function
    // `LSL` based on the first argument via typechecking.
    // In this case the empty {} must be omitted.
    result = LSL(foo, 3);
    return 0;
end;
