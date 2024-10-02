//R_BNCY: The exponentiation operation exp_real(x,y) raises x to the power
//of y. 
//Note:
//Since the real type represents the real numbers, operators such as
//add_real, mul_real, etc. obey the usual mathematical laws such as
//associativity, distributivity, commutativity, etc.

// RUN: interp %s | FileCheck %s
// CHECK: 1000
// CHECK-NEXT: 32

func main() => integer
begin
    print(exp_real(10, 3));
    print(exp_real(2, 5));
    return 0;
end
