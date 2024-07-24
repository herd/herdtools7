//R_NCWM: The exponentiation operation exp_int(x,y) raises x to the power of
//y.
//Note:
//Since the integer type represents the mathematical integers, operators
//such as add_int, mul_int, etc. obey mathematical laws such as
//associativity, distributivity, commutativity, etc.

// RUN: interp %s | FileCheck %s
// CHECK: 1000
// CHECK-NEXT: 32

func main() => integer
begin
    print(exp_int(10, 3));
    print(exp_int(2, 5));
    return 0;
end
