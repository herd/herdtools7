// RUN: interp %s | FileCheck %s
// CHECK: FALSE
// CHECK-NEXT: TRUE
// CHECK-NEXT: 0x5
// CHECK-NEXT: 0x4
// CHECK-NEXT: 0x2
// CHECK-NEXT: 0x1
// CHECK-NEXT: 0x1


func main() => integer
begin
    var a : bits(3) = '101';
    var b : bits(3) = '100';

    print((a == b)); // Legal
    print((a != b)); // Legal
    print((a OR b)); // Legal
    print((a AND b)); // Legal
    print((NOT a)); // Legal
    print((a EOR b)); // Legal
    print((a + b)); // Legal
    print((a - b)); // Legal

    return 0;
end
