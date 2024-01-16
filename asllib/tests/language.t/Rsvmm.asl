// RUN: interp %s | FileCheck %s
// CHECK: 2
// CHECK-NEXT: -2

func main() => integer
begin
    print(fdiv_int(6, 3));
    print(fdiv_int(-5, 3));
    return 0;
end
