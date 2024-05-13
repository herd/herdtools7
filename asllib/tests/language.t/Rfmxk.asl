//R_FMXK: Type T type-satisfies type S if and only if at least one of the
//following conditions holds:
//• T isasubtypeofS
//• T subtype-satisfies S and at least one of S or T is an anonymous type
//• T is an anonymous bitvector with no bitfields and S has the structure of
//a bitvector (with or without bitfields) of the same width as T.

// RUN: interp %s | FileCheck %s

type a of integer;
type b subtypes a;


type c of bits(4) {
    [0] cc,
    [1:0] ccc
};

func main() => integer
begin
    var bb: b;
    var aa: a = bb;

    var i: integer = 10;
    var aaa: a = i;

    var g : bits(4) = '1111';
    var cccc : c = g;

    return 0;
end
