//R_FPMT: A global variable identifier is initialized with the value of the
//initialization expression if an initialization expression is given. The
//initialization expression is evaluated when the global variable identifier
//is initialized.

// RUN: interp %s | FileCheck %s
// CHECK: 30

var a: integer = 10 + 10 + 10;

func main() => integer
begin
    print(a);
    return 0;
end
