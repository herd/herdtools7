var g : MyRecord; // { Other(g) -> Other(MyRecord) }

type MyRecord of record {
    data: bits(WORD_SIZE)
}; // { Other(MyRecord) -> Other(WORD_SIZE) }

constant WORD_SIZE = 64; // { }

// { Other(Color) -> Other(RED),
//   Other(Color) -> Other(GREEN),
//   Other(Color) -> Other(BLUE) }
type Color of enumeration { RED, GREEN, BLUE };

func main() => integer // { Subprogram(main) -> Other(g) }
begin
    var x = g;
    return 0;
end;
