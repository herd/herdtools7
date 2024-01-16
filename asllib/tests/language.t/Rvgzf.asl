// RUN: interp %s | FileCheck %s
// CHECK: 80
// CHECK-NEXT: 96
// CHECK-NEXT: 3
// CHECK-NEXT: 6

func main() => integer
begin
    print(shiftleft_int(10, 3));
    print(shiftleft_int(3, 5));
    print(shiftright_int(100, 5));
    print(shiftright_int(50, 3));
    return 0;
end
