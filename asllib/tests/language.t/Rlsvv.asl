// RUN: interp %s | FileCheck %s
// CHECK: 1
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
    for x = 1 to 10 do
        print(x);
    end
    return 0;
end
