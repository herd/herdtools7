//R_GBNC: A global variable identifier is initialized with an arbitrary
//value of the variable’s type if no initialization expression is given.

// RUN: interp %s | FileCheck %s
// CHECK: 0

var a: integer;

func main() => integer
begin
    print(a);

    return 0;
end
