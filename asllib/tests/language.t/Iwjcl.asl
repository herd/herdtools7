// RUN: interp %s | FileCheck %s
// CHECK: 1
// CHECK-NEXT: 2

func main() => integer
begin
    var a: real = 1.5;
    var b: integer = RoundDown(a);
    var c: integer = RoundUp(a);

    print(b);
    print(c);
    return 0;
end
