// RUN: interp %s | FileCheck %s
// CHECK: 10
// CHECK-NEXT: 5
// CHECK-NEXT: 3

func main() => integer
begin
    var a = if TRUE then 10 else 5;
    print(a);

    var b = if FALSE then 10 else 5;
    print(b);

    var t = a + b;

    var c = if t == 20 then 1 elsif t == 10 then 2 elsif t == 15 then 3 else 4;
    print(c);
    return 0;
end
