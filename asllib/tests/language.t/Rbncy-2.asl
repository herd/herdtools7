//R_BNCY: The exponentiation operation exp_real(x,y) raises x to the power
//of y. 
//Note:
//Since the real type represents the real numbers, operators such as
//add_real, mul_real, etc. obey the usual mathematical laws such as
//associativity, distributivity, commutativity, etc.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (10 ^ 3) == 1000;
    assert (2  ^ 5) == 32;
    return 0;
end
