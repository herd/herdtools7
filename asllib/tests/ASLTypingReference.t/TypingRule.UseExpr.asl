constant FIVE = 5;
constant SEVEN = 7;
var g = 3;

func add_3(x: integer) => integer
begin
    return x + 3;
end;

type MyRecord of record { data: bits(8) };

func main() => integer
begin
    var - = 5; // { }
    var - = SEVEN as integer{FIVE..FIVE*2}; // { Other(SEVEN), Other(FIVE) }
    var - = g; // { Other(g) }
    var arr : array[[10]] of integer;
    var - = arr[[FIVE]]; // { Other(arr), other(FIVE) }
    var - = FIVE + SEVEN; // { Other(SEVEN), Other(FIVE) }
    var - = add_3(FIVE); // { Subprogram(add_3), Other(FIVE) }
    var - = (FIVE, SEVEN).item0; // { Other(SEVEN), Other(FIVE) }
    var r : MyRecord;
    var - = r.data; // { Other(r) }
    var - = ARBITRARY : MyRecord; // { Other(MyRecord) }
    var - = 5 IN { FIVE, SEVEN }; // { Other(SEVEN), Other(FIVE) }
    return 0;
end;
