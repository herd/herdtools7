// RUN: interp %s | FileCheck %s
// CHECK: 0
// CHECK-NEXT: 0

func main() => integer
begin
    var a: array[2] of integer;
    print(a[0]);
    print(a[1]);
    return 0;
end
