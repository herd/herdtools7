func main() => integer
begin
    var result : bits(64);
    // Illegal: an empty parameter list for a standard library
    // function must be omitted.
    result = LSL{}(result, 3);
    return 0;
end;
