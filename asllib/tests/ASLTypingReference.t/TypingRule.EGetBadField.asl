func main() => integer
begin
    var a: array[[5]] of integer;
    // The next statement is illegal as 'a' not a bitvector type,
    // a structured type, or a tuple type.
    var - = a.f;
    return 0;
end;
