//R_ZJSH: The type of a bitfield which does not have a type annotation is a
//bitvector type of the width of the bitfield.

// RUN: interp %s | FileCheck %s

type a of bits(6) {
    [4:0] aa,
};

func main() => integer
begin
    var b: a;
    var c: bits(5) = b.aa;
    return 0;
end
