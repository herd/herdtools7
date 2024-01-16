// RUN: interp %s | FileCheck %s
// CHECK: 0
// CHECK-NEXT: 0
func main() => integer
begin
    var a: (integer, integer);
    var (b, c) = a;
    print(b);
    print(c);
    return 0;
end
