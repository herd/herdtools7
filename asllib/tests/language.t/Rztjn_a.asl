//R_ZTJN: The operation div_int performs exact division. The divisor (the
//second operand) must be a positive integer that exactly divides the first
//operand.

// RUN: interp %s | FileCheck %s
// CHECK: 2
// CHECK-NEXT: -2

func main() => integer
begin
    print(div_int(6, 3));
    print(div_int(-6, 3));
    return 0;
end
