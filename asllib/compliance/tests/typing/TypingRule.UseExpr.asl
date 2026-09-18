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
    - = 5; // { }
    - = SEVEN as integer{FIVE..FIVE*2}; // { Other(SEVEN), Other(FIVE) }
    - = g; // { Other(g) }
    var arr : array[[10]] of integer;
    - = arr[[FIVE]]; // { Other(arr), other(FIVE) }
    - = FIVE + SEVEN; // { Other(SEVEN), Other(FIVE) }
    - = add_3(FIVE); // { Subprogram(add_3), Other(FIVE) }
    - = (FIVE, SEVEN).item0; // { Other(SEVEN), Other(FIVE) }
    var r : MyRecord;
    - = r.data; // { Other(r) }
    - = ARBITRARY : MyRecord; // { Other(MyRecord) }
    - = 5 IN { FIVE, SEVEN }; // { Other(SEVEN), Other(FIVE) }
    return 0;
end;
