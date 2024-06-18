//R_CPCK: The base value of a boolean is FALSE.

// RUN: interp %s | FileCheck %s
// CHECK: FALSE

func main() => integer
begin
    var a: boolean;
    print(a);
    return 0;
end
