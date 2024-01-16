// RUN: interp %s | FileCheck %s
// CHECK: FALSE

func main() => integer
begin
    var a: boolean;
    print(a);
    return 0;
end
