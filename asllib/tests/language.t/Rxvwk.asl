//R_XVWK: If no arguments of a bitvector concatenation with n arguments are
//under-constrained width bitvectors and at least one argument is a
//constrained width bitvector then the result of the concatenation is a
//constrained width bitvector whose constraint is C which is calculated as
//follows:
//- if the i’th argument is a constrained width bitvector, then the set Ci
//is the constraint of the argument’s width 
//- if the i’th argument is a fixed width bitvector, then Ci is the set
//containing only the argument’s width
//- thesetCis{x|y1 ∈C1 ∧···∧yn ∈Cn ∧x=Pni=1yi}


// RUN: interp %s | FileCheck %s

func f(P: integer {2,4,8})
begin
    var opA = [Zeros(P-1), '1'];
    // First element of concatenation is a constrained width bitvector of determined width
    // with constraint {1,3,7} and width (P-1)
    // Second element of concatenation is a fixed width bitvector of width 1
    // For the purpose of calculating the resulting type, it has constraint {1}
    // The result is a constrained width bitvector of determined width
    // with constraint {1+1,3+1,7+1} and width ((P-1)+1)
    // Hence the type of opA is bits(P as {2,4,8})
end

func main() => integer
begin
    return 0;
end
