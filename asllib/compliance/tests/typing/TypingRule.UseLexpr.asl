constant FIVE = 5;
constant SEVEN = 7;
var g1 : integer = 3;
var g2 : integer = 3;

func add_3(x: integer) => integer
begin
    return x + 3;
end;

type MyRecord of record { data: bits(8) };

func main() => integer
begin
    g1 = 5; // { Other{g1} }
    (g1, g2) = (9, 9); // { Other{g1}, Other(g2) }
    - = 9; // { }
    var arr : array[[10]] of integer;
    arr[[g1]] = 6; // { Other(arr), Other(g1) }
    var r : MyRecord;
    r.data[SEVEN:FIVE] = Ones{3}; // { Other(r), Other(SEVEN), Other(FIVE) }
    return 0;
end;
