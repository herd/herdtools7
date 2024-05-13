//I_HMRK: For example, the base value of a tuple (integer, boolean) is the
//tuple (0, FALSE).

// RUN: interp %s | FileCheck %s
// CHECK: 0
// CHECK-NEXT: FALSE

func main() => integer
begin
    var a: (integer, boolean);
    var (b, c) = a;
    print(b);
    print(c);
    return 0;
end
