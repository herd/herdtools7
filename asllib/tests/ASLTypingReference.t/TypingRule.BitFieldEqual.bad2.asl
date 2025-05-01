func main() => integer
begin
    // Illegal: bitfields of the same name must have the same
    // set of nested bitfields.
    var x : bits(64) { [16+:16] data { [0] lsb } } =  Zeros{64} as
            bits(64) { [31:16] data {  } };
    return 0;
end;
