type MyRecord of record { status: bit, time: bits(16), data: bits(8) };
type Message of bits(25) { [0] status, [16:1] time, [24:17] data };

func main() => integer
begin
    var x : MyRecord;
    x.[status, time, data] = '1' :: Zeros{16} :: Ones{8};
    assert x.status :: x.time :: x.data == '1 0000000000000000 11111111';

    var y : Message;
    assert y == '00000000 0000000000000000 0';
    y.[data, time, status] = Ones{8} :: Zeros{16} :: '1';
    assert y == '11111111 0000000000000000 1';
    return 0;
end;
