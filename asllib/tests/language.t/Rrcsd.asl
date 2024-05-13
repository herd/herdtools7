//R_RCSD: It is an error if any field used is not of type bitvector.

// RUN: not interp %s | FileCheck %s

type a of record {
    aa: bits(1),
    bb: bits(2),
    cc: integer
};

func main() => integer
begin
    var aa: a;
    var bb: bits({1..10}) = aa.[aa, bb, cc];
    return 0;
end
