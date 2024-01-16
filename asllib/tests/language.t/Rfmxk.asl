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
