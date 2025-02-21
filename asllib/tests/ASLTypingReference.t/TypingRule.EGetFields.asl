type BitvectorType of bits(8) {
    [0] bit0,
    [1] bit1,
    [3:2] bits3_2,
    [7:6] info,
    [7:6, 1:0] info_and_bits
};

type RecordWithBits of record {
    bit0: bits(1),
    bit1: bits(1),
    bits3_2: bits(2),
    info: bits(2),
    r: real
};

func main() => integer
begin
    var bits_var : BitvectorType = '10100010';
    //              Multi-field expression                          inferred type
    let bits_flds = bits_var.[bits3_2, bit1, bit0, info_and_bits]   as bits(8);
    assert bits_flds == '00101010';

    var record_var : RecordWithBits = RecordWithBits{
        bit0    = '0',
        bit1    = '1',
        bits3_2 = '00',
        info    = '10',
        r       = 6.7
    };
    //                  Multi-field expression                      inferred type
    let record_flds =   record_var.[bits3_2, bit1, bit0, info]      as bits(6);
    assert record_flds == '001010';
    return 0;
end;
