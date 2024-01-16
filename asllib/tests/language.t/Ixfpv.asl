// RUN: not interp %s | FileCheck %s
// CHECK: 10
// CHECK-NEXT: 20

var a = (10, 20);

func main() => integer
begin
    var c = a.item2;

    return 0;
end
