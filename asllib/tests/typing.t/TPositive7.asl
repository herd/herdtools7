type MyBitsSizes  of integer {8,16,32};
type MyOtherSizes of integer {8,16,32};
type MyByteSizes  of integer {1,2,4};

func positive7(size : MyBitsSizes, size2 : integer {8,16,32})
begin
    let testA : MyBitsSizes       = size;
    let testB : MyBitsSizes       = size2;                // assignment to/from unnamed types is permitted, as long as the constraints are satisfied
    let testC : MyBitsSizes       = 8;
    let testD : integer {8,16,32} = size;
    let testE : MyByteSizes       = size DIV 8;           // operators erase the name of the type. The type of "size DIV 8" is therefore just
                                                          // integer {1,2,4}, and can therefore be assigned to a var of type MyByteSizes
    let testF : MyOtherSizes      = size as MyOtherSizes; // ATC's can be used between named types of the same structure
    let testG : MyByteSizes       = size as MyByteSizes;  // As per positive5 and ASL-313, ATC's can be used even if the domains are disjoint. This
                                                          // must be valid statically but will fail at runtime if the line is reached.
end
