// RUN: interp %s | FileCheck %s
// CHECK: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 5
// CHECK-NEXT: 6
// CHECK-NEXT: 7
// CHECK-NEXT: 8
// CHECK-NEXT: 9
// CHECK-NEXT: 10

func main() => integer
begin
    var a : integer = 0;
    repeat
        print(a);
        a = a + 1;
    until a > 10;
    return 0;
end
