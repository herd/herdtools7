// RUN: interp %s | FileCheck %s
// CHECK: 3.141590e+0

func main() => integer
begin
    var a: real = 3.14159;
    print(a);
    return 0;
end
