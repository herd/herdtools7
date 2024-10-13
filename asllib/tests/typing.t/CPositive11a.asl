// This function is statically legal, but it is a dynamic error if the function is invoked with N != 59.
func positive11a{N: integer {0..64}} (x: bits(N)) => bits(64)
begin
    return 
        Ones(5) as bits(64 - N) ::
        x
    ; // has static width of bits(64)
end
