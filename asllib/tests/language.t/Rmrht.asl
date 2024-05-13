//R_MRHT: If the arguments of a comparison operation are bitvectors then
//they must have the same determined width.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(1);
    var b: bits(11);
    var c = a == b;

    return 0;
end
