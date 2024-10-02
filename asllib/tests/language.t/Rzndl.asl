//R_ZNDL: The IN operator is equivalent to testing its first operand for
//equality against each value in the (possibly infinite) set denoted by the
//second operand, and taking the logical OR of the result. Values denoted by
//a set of patterns comprise the union of the set of values denoted by each
//pattern. Values denoted by a bitmask_lit comprise all bitvectors that
//could match the bit-mask. It is not an error if any or all of the values
//denoted by the first operand can be statically determined to never compare
//equal with the second operand.

// RUN: interp %s | FileCheck %s
// CHECK-: TRUE
// CHECK-NEXT: TRUE
// CHECK-NEXT: FALSE
// CHECK-NEXT: TRUE
// CHECK-NEXT: FALSE
// CHECK-NEXT: TRUE
// CHECK-NEXT: TRUE
// CHECK-NEXT: TRUE
// CHECK-NEXT: TRUE
// CHECK-NEXT: FALSE

func main() => integer
begin
    let expr_A = '111' IN {'1xx'};
    print(expr_A);
    let expr_Aa = '111' IN '1xx';
    print(expr_Aa);
    let expr_B = '111' IN {'0xx'};
    print(expr_B);
    let expr_C = 3 IN {2,3,4};
    print(expr_C);
    let expr_D = 1 IN {2,3,4};
    print(expr_D);
    let expr_E = 3 IN {1..10};
    print(expr_E);
    let expr_F = 3 IN {<= 10};
    print(expr_F);
    let expr_G = 3 IN !{1,2,4};
    print(expr_G);
    let expr_H = (1,'10') IN {(1,'1x')};
    print(expr_H);
    let expr_I = (1,'10') IN {(1,'0x'), (2, '1x')};
    print(expr_I);

    return 0;
end
