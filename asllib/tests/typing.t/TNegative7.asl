type MyBitsSizes  of integer {8,16,32};
type MyOtherSizes of integer {8,16,32};
type MyByteSizes  of integer {1,2,4};

func negative7(size : MyBitsSizes)
begin
    let testA : MyOtherSizes = size; // illegal as testA and size are different named types, even though they are the same structure and domain
end

