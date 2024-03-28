// RUN: interp %s | FileCheck %s
// CHECK: 30

var a: integer = 10 + 10 + 10;

func main() => integer
begin
    print(a);
    return 0;
end
