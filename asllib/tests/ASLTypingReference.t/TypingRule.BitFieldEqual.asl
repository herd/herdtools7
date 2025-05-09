func main() => integer
begin
    var x : bits(64) { [16+:16] data } = Zeros{64} as bits(64) { [31:16] data };

    var y : bits(64) { [16+:16] data { [0] lsb } } =  Zeros{64} as
            bits(64) { [31:16] data { [0] lsb } };

    var z : bits(64) { [0] lsb : bits(1) } = Zeros{64} as bits(64) { [0] lsb : bit };
    return 0;
end;
