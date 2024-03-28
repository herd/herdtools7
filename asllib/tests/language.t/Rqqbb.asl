// RUN: interp %s | FileCheck %s
// CHECK: 10.0
// CHECK-NEXT: 10.0
// CHECK-NEXT: 10.0
// CHECK-NEXT: 10.0
// CHECK-NEXT: 10.0
// CHECK-NEXT: 10.0

func main() => integer
begin
    print(10.0);
    print(1_0.0);
    print(10.0_);
    print(10.00);
    print(10_.0);
    print(10.0_0);

    return 0;
end
