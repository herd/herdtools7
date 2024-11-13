config LargeConfigurableBitVectorSize : integer{1..0xFFFF_FFFF_FFFF_FFFF} = 8;
type LargeBitvectorSize of integer{0..LargeConfigurableBitVectorSize};

func CreateBigVector(bytes: LargeBitvectorSize) => bits(8*bytes)
begin
    var value = Zeros (8*bytes);
    value = Ones(8*bytes);
    return value;
end;
