//R_KSZM: A local storage element declared with var is initialized with the
//base value of its type if no initialization expression is given.

// RUN: interp %s | FileCheck %s
// CHECK: 0

func main() => integer
begin
    var a: integer;

    print(a);

    return 0;
end
