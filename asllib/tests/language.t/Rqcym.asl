//R_QCYM: The type of a bitfield with a type annotation must subtype-satisfy
//the bitvector type with size of the width of the bitfield.

// RUN: interp %s | FileCheck %s

type b of bits(5);

type a of bits(6) {
    [4:0] aa : b,
};

func main() => integer
begin
    return 0;
end
