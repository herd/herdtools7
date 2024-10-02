//D_WSXY: Multi-assignment is a destructuring assignment that allows the
//elements of a tuple on the right-hand side of an assignment to be
//stored in different storage elements as denoted by the items in the
//parenthesized list on the left-hand side.

// RUN: interp %s | FileCheck %s
// CHECK: 1
// CHECK-NEXT: 2

func main() => integer
begin
    var (a, b) = (1, 2);

    print(a);
    print(b);

    return 0;
end
