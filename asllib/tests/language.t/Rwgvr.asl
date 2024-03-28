// RUN: interp %s | FileCheck %s
// CHECK: 0

func main() => integer
begin
    var a: array[10] of integer;
    print(a[0]);
    return 0;
end
