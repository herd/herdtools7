//R_QXGW: The type bit is identical to the type bits(1)

// RUN: interp %s | FileCheck %s
// CHECK: TRUE

func main() => integer
begin
    var a: bit;
    var b: bits(1);

    print((a == b));
    return 0;
end
