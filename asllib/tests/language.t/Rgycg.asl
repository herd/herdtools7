//R_GYCG: The base value of the real type is 0.0.

// RUN: interp %s | FileCheck %s
// CHECK: 0.0

func main() => integer
begin
    var a: real;
    print(a);
    return 0;
end
