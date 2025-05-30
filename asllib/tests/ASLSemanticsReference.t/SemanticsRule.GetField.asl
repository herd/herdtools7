type Color of enumeration {RED, GREEN, BLUE};

type MyRecord of record {
    RED: integer,
    GREEN: integer,
    BLUE: integer,
};

func main() => integer
begin
    var color_to_int: array[[Color]] of integer;
    color_to_int[[RED]] = 0;
    color_to_int[[GREEN]] = 1;
    color_to_int[[BLUE]] = 2;
    assert color_to_int[[RED]] == 0;
    assert color_to_int[[GREEN]] == 1;
    assert color_to_int[[BLUE]] == 2;

    var r: MyRecord = MyRecord{RED = 0, GREEN = 1, BLUE = 2};
    assert r.RED == 0;
    assert r.GREEN == 1;
    assert r.BLUE == 2;
    r.BLUE = -1;
    return 0;
end;
