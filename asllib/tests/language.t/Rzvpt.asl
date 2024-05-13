//R_ZVPT: The base value of bits(N) is the N-bit bitvector containing only
//zeros.

// RUN: interp %s | FileCheck %s
// CHECK: 0x0

func main() => integer
begin
    var a: bits(32);
    print(a);
    return 0;
end
