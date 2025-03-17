// { Other(status) }
type RecordBase of record {status: boolean};

// { }
constant HALF_WORD_BITS = 8;

// { Other(RecordBase), Other(HALF_WORD_BITS) }
type MyRecord subtypes RecordBase with { data: bits(HALF_WORD_BITS) };

// { }
constant WORD_BITS = 16;

// { Subprogram(Zeros), Other(WORD_BITS) }
var g : bits(WORD_BITS) = Zeros{WORD_BITS};

// { Subprogram(Ones), Other(bv), Other(res) }
func flip{N}(bv: bits(N)) => bits(N)
begin
    let res = Ones{N} XOR bv;
    return res;
end;

// { }
func main() => integer
begin
    return 0;
end;
