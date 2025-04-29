type Packet of bits(8) {
    [0] flag,
    [7:1] data,
    [7:1] detailed_data {
        [6:3] info : bits(4) {
            [0] info_0
        },
        [2:0] crc
    }
};

func main() => integer
begin
    var p : Packet;
    //      Bitfield expression         inferred type
    - = p.flag                  as  bits(1);
    - = p.data                  as  bits(7);
    - = p.detailed_data         as  bits(7) { [3+:4] info, [0+:3] crc };
    - = p.detailed_data.info    as  bits(4) { [0] info_0 };
    return 0;
end;
