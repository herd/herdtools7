// RUN: interp %s | FileCheck %s
// CHECK-NOT: 1

func main() => integer
begin
    var a : integer = 0;
    repeat
        print(a);
        a = a + 1;
    until TRUE;
    return 0;
end
