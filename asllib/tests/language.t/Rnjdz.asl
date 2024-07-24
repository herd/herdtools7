//R_NJDZ: The base value of the unconstrained integer is 0.

// RUN: interp %s | FileCheck %s
// CHECK: 0

func main() => integer
begin
    var a: integer;
    print(a);
    return 0;
end
