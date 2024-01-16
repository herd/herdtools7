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
    let expr_Aa = '111' IN '1xx';
    let expr_B = '111' IN {'0xx'};
    let expr_C = 3 IN {2,3,4};
    let expr_D = 1 IN {2,3,4};
    let expr_E = 3 IN {1..10};
    let expr_F = 3 IN {<= 10};
    let expr_G = 3 IN !{1,2,4};
    let expr_H = (1,'10') IN {(1,'1x')};
    let expr_I = (1,'10') IN {(1,'0x'), (2, '1x')};

    return 0;
end
