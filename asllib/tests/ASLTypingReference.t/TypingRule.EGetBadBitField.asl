type Packet of bits(8) { [0] flag, [7:1] data };

func main() => integer
begin
    var p : Packet;
    // Illegal: field 'undeclared_bitfield' is not declared for Packet.
    var - = p.undeclared_bitfield;
    return 0;
end;
