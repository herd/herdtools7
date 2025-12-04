type Packet of record { data: bits(8), status: boolean};
type ExtendedPacket subtypes Packet with {time: integer};

func main() => integer
begin
    // Illegal: cannot perform ATC to convert between real and integer types.
    var a = 3.0 as integer{1, 2};

    // Illegal: cannot perform ATC on record types unless they
    // are equivalent.
    var rec = Packet{data = Zeros{8}, status = TRUE};
    var b = rec as ExtendedPacket;

    var arr : array[[5]] of integer;
    // Illegal: cannot perform ATC on array types unless they
    // are equivalent.
    var c = arr as array[[6]] of integer;
    return 0;
end;
