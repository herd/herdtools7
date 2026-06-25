type Color of enumeration {RED, GREEN, BLUE};

type MyRecord of record {
    RED: integer,
    GREEN: integer,
    BLUE: integer,
};

func main() => integer
begin
    var r: MyRecord = MyRecord{RED = 0, GREEN = 1, BLUE = 2};
    assert r.RED == 0;
    assert r.GREEN == 1;
    assert r.BLUE == 2;
    r.BLUE = -1;
    return 0;
end;
