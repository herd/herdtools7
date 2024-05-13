//R_LRHD: The operator <-> represents the boolean if and only if operator.
//Conditional expressions and the operations and_bool or_bool and
//implies_bool provide a short-circuit evaluation similar to the C
//programming language. That is, the first operand of if is always evaluated
//but only one of the remaining operands is evaluated; if the first operand
//of and_bool is FALSE, then the second operand is not evaluated; if the
//first operand of or_bool is TRUE, then the second operand is not
//evaluated; and, if the first operand of implies_bool is FALSE, then the
//second operand is not evaluated. We note that relying on this
//short-circuit evaluation can be confusing for readers and it is
//recommended that an if-statement is used to achieve the same effect.


// RUN: interp %s | FileCheck %s
// CHECK: TRUE
// CHECK-NEXT: FALSE
// CHECK-NEXT: FALSE
// CHECK-NEXT: TRUE


func main() => integer
begin
    print(FALSE <-> FALSE);
    print(FALSE <-> TRUE);
    print(TRUE <-> FALSE);
    print(TRUE <-> TRUE);

    return 0;
end
