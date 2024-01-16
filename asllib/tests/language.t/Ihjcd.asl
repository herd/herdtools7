// RUN: interp %s | FileCheck %s
// CHECK: 1000000
// CHECK-NEXT: 1000000

func main() => integer
begin
    print(1000000);
    print(1_000_000);
    return 0;
end
