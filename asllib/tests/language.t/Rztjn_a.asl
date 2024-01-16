// RUN: interp %s | FileCheck %s
// CHECK: 2
// CHECK-NEXT: -2

func main() => integer
begin
    print(div_int(6, 3));
    print(div_int(-6, 3));
    return 0;
end
