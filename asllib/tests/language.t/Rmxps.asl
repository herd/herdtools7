// RUN: interp %s | FileCheck %s
// CHECK: FALSE
// CHECK-NEXT: TRUE

func main() => integer
begin
    print(FALSE);
    print(TRUE);
    return 0;
end
