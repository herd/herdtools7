type Color of enumeration {RED, GREEN, BLUE}; // { }

type MyRecord of record { c: Color }; // { Other(Color) }

constant c = 15; // { }

func foo{N}(bv: bits(N)) => integer
begin
    var - : Color; // { Other(Color) }
    var - : boolean; // { }
    var - : real; // { }
    var - : string; // { }
    var - : integer; // { }
    var - : integer{0..N} = N as integer{0..N}; // { Other(N) }
    var - : (integer{0..N}, boolean) = (0 as integer{0..N}, TRUE); // { Other(N) }
    var - : MyRecord; // { Other(MyRecord) }
    var - : array[[N]] of MyRecord; // { Other(N), Other(MyRecord) }
    var - : array[[Color]] of MyRecord; // { Other(Color), Other(MyRecord) }
    var - : bits(64) { [c] flag }; // { Other(c) }
    var - : bits(N) = Zeros{N}; // { Other(N) }

    return 0;
end;
