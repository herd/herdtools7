//R_NCWM: The exponentiation operation exp_int(x,y) raises x to the power of
//y.
//Note:
//Since the integer type represents the mathematical integers, operators
//such as add_int, mul_int, etc. obey mathematical laws such as
//associativity, distributivity, commutativity, etc.
// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (10 ^ 3) == 1000;
    assert (2 ^ 5)  == 32;
    return 0;
end
