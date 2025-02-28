type Packet of record { data: bits(8), status: boolean};
type ExtendedPacket subtypes Packet with {time: integer};

func main() => integer
begin
    // Illegal: can only perform ATC on real and integer:
    // ATC is not a type-to-type cast.
    var - = 3.0 as integer{1, 2};

    // Illegal: cannot perform ATC on record types unless they
    // are exactly they are equivalent.
    var rec = Packet{data = Zeros{8}, status = TRUE};
    var - = rec as ExtendedPacket;

    var arr : array[[5]] of integer;
    // Illegal: cannot perform ATC on array types unless they
    // are equivalent.
    var - = arr as array[[6]] of integer;
    return 0;
end;
