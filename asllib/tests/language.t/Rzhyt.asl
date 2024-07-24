//R_ZHYT: It is an error if any field in a multiple field assignment is not
//of type bitvector.

// RUN: interp %s | FileCheck %s

type a of record {
    aa: bits(2),
    bb: bits(2)
};


func main() => integer
begin
    var b: a;
    b.[aa, bb] = '1111';
    return 0;
end
