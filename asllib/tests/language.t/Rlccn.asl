// RUN: interp %s | FileCheck %s
// CHECK: TRUE
type enum of enumeration{A, B};

func main() => integer
begin
    var a: enum;
    print((a == A));
    return 0;
end
