// RUN: interp %s | FileCheck %s
// CHECK: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10

func main() => integer
begin
    print(10);
    print(1_0);
    print(0xa);
    print(0xA);
    print(0xA_);
    print(0x0a);

    return 0;
end
