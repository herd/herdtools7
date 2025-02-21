type Color of enumeration {RED, GREEN, BLUE};
type SubColor subtypes Color;

type Packet of record { data: bits(8), status: boolean};
type ExtendedPacket subtypes Packet;

func main() => integer
begin
    //      Expression                          Annotated expression
    // The following assertion is statically proved by the type
    // system and therefore no dynamic check occurs.
    var - = 1 as integer{1, 2};                     // 1
    // The following assertion is not statically proved by the type
    // system and therefore a dynamic check occurs, which always fails.
    var - = 3 as integer{1, 2};                     // 3 as integer{1, 2}
    var - = 3 as integer{2 as integer{2, 3}};       // 3 as integer{2}

    var - = RED as Color;                           // RED
    var - = RED as SubColor;                        // RED
    var - = (RED, 3) as (SubColor, integer{2, 3});  // (RED, 3)
    // The following right-hand-side expression is annotated as
    // (RED, 3) as (enumeration {RED, GREEN, BLUE}, integer {1, 2})
    var - = (RED, 3) as (SubColor, integer{1, 2});

    var x = Packet{data = Zeros{8}, status = TRUE};
    var - = x as ExtendedPacket;                    // x

    var arr : array[[5]] of integer;
    var - = arr as array[[5]] of integer;           // arr

    // The following is illegal as '2 as integer{3}'
    // is considered side-effecting, which is not allowed in type
    // definitions.
    // var - = 3 as integer{2 as integer{3}};
    return 0;
end;
