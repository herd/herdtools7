//R_GHXR: The remainder operation frem_int is defined as the remainder of
//division rounding towards negative infinity. The second operand must be a
//positive integer.

// RUN: interp %s | FileCheck %s
// CHECK: 0
// CHECK-NEXT: 1

func main() => integer
begin
    print(frem_int(6, 3));
    print(frem_int(-5, 3));
    return 0;
end
