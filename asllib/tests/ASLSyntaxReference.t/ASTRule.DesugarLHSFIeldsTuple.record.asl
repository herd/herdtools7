type MyRecord of record {
    i: integer,
    r: real,
    bv: bits(32),
    b: boolean
};

func main() => integer
begin
    var x: MyRecord;
    x.(i, -, bv, -) = (5, 3.2, Zeros{32}, TRUE);
    return 0;
end;
