//D_WXQV: The bits(N) type represents a bitvector of length N, where N
//may specify a fixed width or a constrained width.

// RUN: interp %s | FileCheck %s

config a: integer{0..10} = 4;

func main() => integer
begin
    var b: bits(10);
    var c: bits(a);
    return 0;
end
