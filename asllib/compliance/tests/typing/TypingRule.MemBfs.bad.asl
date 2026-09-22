type FlaggedPacket of bits(8) {
    [7:1] data,
    [0] flag
};

func main() => integer
begin
    var y: FlaggedPacket;
    var x : bits(8) {[0] flag, [1] lsb} = y;
    return 0;
end;
