// RUN: interp %s | FileCheck %s
// CHECK: 1
// CHECK_NEXT: 2
// CHECK_NEXT: 4
// CHECK_NEXT: 8

func main() => integer
begin
    var a : integer = 1;
    while a < 10 do
        print(a);
        a = 2 * a;
    end
    return 0;
end
