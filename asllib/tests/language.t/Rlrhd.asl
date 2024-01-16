// RUN: interp %s | FileCheck %s
// CHECK: TRUE
// CHECK-NEXT: FALSE
// CHECK-NEXT: FALSE
// CHECK-NEXT: TRUE


func main() => integer
begin
    print(FALSE <-> FALSE);
    print(FALSE <-> TRUE);
    print(TRUE <-> FALSE);
    print(TRUE <-> TRUE);

    return 0;
end
