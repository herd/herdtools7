// RUN: interp %s | FileCheck %s
// CHECK: TRUE
// CHECK-NEXT: TRUE
func main() => integer
begin
    var c = '10010';

    print((c IN '10x10'));
    print((c IN '10 x 10'));
    return 0;
end
