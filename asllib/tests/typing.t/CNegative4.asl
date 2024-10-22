// width of return value is not statically equivalent to width of return type
// (a checked type conversion is required)
func negative4{N: integer {0..64}} (x: bits(N)) => bits(64)
begin
    return 
        Ones(5) ::
        x
    ;
end
