// RUN: interp %s | FileCheck %s
// CHECK: hello
// CHECK-NEXT: wor"ld
// CHECK-NEXT: te\st
// CHECK-NEXT: bre
// CHECK-NEXT: ak

func main() => integer
begin
    print("hello");
    print("wor\"ld");
    print("te\\st");
    print("bre\nak");
    return 0;
end
