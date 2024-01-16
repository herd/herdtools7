// RUN: interp %s | FileCheck %s
// CHECK: TRUE

func main() => integer
begin
    var a: bit;
    var b: bit(1);

    print((a == b));
    return 0;
end
