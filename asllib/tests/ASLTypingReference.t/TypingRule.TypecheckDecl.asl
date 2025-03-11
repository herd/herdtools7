type MyRecord of record {
    high_bits: bits(32),
    low_bits: bits(32),
};

type MyException of exception {
    msg: string,
};

type MyCollection of collection {
    high_bits: bits(32),
    low_bits: bits(32),
};

var rec: MyRecord;
var exc: MyException;
var coll: MyCollection;

accessor Rec() <=> bits (64)
begin
    getter begin
        return rec.high_bits :: rec.low_bits;
    end;

    setter = values begin
        rec.high_bits = values[63:32];
        rec.low_bits = values[31:0];
    end;
end;

func main() => integer
begin
    println(Rec());
    Rec() = Ones{64};
    println(Rec());
    return 0;
end;
