type Color of enumeration {RED, GREEN, BLUE}; // { }

type MyRecord of record { c: Color }; // { Other(Color) }

constant FIFTEEN = 15; // { }

func foo{N}(bv: bits(N)) => integer
begin
    var a : Color; // { Other(Color) }
    var b : boolean; // { }
    var c : real; // { }
    var d : string; // { }
    var e : integer; // { }
    var f : integer{0..N} = N as integer{0..N}; // { Other(N) }
    var g : (integer{0..N}, boolean) = (0 as integer{0..N}, TRUE); // { Other(N) }
    var h : MyRecord; // { Other(MyRecord) }
    var i : array[[N]] of MyRecord; // { Other(N), Other(MyRecord) }
    var j : array[[Color]] of MyRecord; // { Other(Color), Other(MyRecord) }
    var k : bits(64) { [FIFTEEN] flag }; // { Other(FIFTEEN) }
    var l : bits(N) = Zeros{N}; // { Other(N) }

    return 0;
end;
