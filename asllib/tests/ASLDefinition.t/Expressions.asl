var y: integer{1..10};

func ReturnSeven() => integer{7}
begin
    return 7;
end;

type MyRecord of record {val: integer};

func main() => integer
begin
    // Example of a binary expression and an asserting type conversion expression.
    var x = y * 3 as integer{0..1000};

    // Example of a conditional expression, a binary relational integer expression,
    // a call expression, and an arbitrary value expression.
    var z = if x > 9 then ReturnSeven() else (ARBITRARY: integer);
    var rec: MyRecord;

    // Example of a record construction expression and a field reading expression.
    rec.val = MyRecord{val=5}.val + 1;

    // Example of a tuple expression.
    (x, z) = (7, 9);
    var bv: bits(8);

    // Example of a bitvector slicing expression.
    bv = Ones{10}[:8];
    return 0;
end;
