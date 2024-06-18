//R_CFTD: The base value of a well-constrained integer is the closest value
//to zero in its domain. If the closest positive and negative value are
//equally close, the positive value is used.

// RUN: interp %s | FileCheck %s
// CHECK: 4
// CHECK-NEXT: -4
// CHECK-NEXT: 4

func main() => integer
begin
    var a: integer{4..10};
    var b: integer{-10..-4};
    var c: integer{-10..-4, 4..10};

    print(a);
    print(b);
    print(c);

    return 0;
end
