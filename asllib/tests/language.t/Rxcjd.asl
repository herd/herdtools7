//R_XCJD: There is no bound on the minimum and maximum absolute real value
//that can be represented.

// RUN: interp %s | FileCheck %s
// CHECK: 1.000000e+69
// CHECK-NEXT: -1.000000e+69
// CHECK-NEXT: 1.000000e-70

func main() => integer
begin
    var a: real = 1000000000000000000000000000000000000000000000000000000000000000000000.0;
    var b: real = -1000000000000000000000000000000000000000000000000000000000000000000000.0;
    var c: real = 0.0000000000000000000000000000000000000000000000000000000000000000000001;

    print(a);
    print(b);
    print(c);
    return 0;
end