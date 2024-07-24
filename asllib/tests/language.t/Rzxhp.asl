//R_ZXHP: The initialization expression in a let or var declaration is
//evaluated during execution-time at the point of declaration, hence any
//side effects occur at the point of declaration.

// RUN: interp %s | FileCheck %s
// CHECK: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3

func test() => integer
begin
    print(2);
    return 3;
end

func main() => integer
begin
    print(1);
    var a = test();
    print(a);
    return 0;
end
