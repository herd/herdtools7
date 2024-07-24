// RUN: interp %s | FileCheck %s
// CHECK: 5
// CHECK-NEXT: 5
// CHECK-NEXT: 5

func a(aa: integer)
begin
    print(aa);
end

func main() => integer
begin
    var bb = 5;
    print(bb);
    a(bb);
    print(bb);
    return 0;
end
