func ZerosBytes(N : integer {1,2,4,8}) => bits(N*8)
begin
    return Zeros(N*8);
end

func positive11(size : bits(2), width : integer {1..3})
begin
    let sizeInt                          = UInt(size);
    let numBytes                         = 1 << sizeInt;
    let numBits                          = 8 << sizeInt;
    let testA : bits(numBits)            = ZerosBytes(numBytes);

    let esize                            = 8 << UInt(size);
    let testB : bits(esize)              = ZerosBytes(esize DIV 8);

    let testC : integer {0..(2^esize)-1} = UInt(testB); // symbolically constrained integer, without being an under defined integer
    let testD : bits(testC)              = Zeros(testC);

    let tempE                            = width + sizeInt;
    let testE : bits(tempE)              = Zeros(sizeInt) :: Zeros(width);
end
