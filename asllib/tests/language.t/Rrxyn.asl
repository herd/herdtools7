// RUN: interp %s | FileCheck %s
// CHECK: 240
// CHECK-NEXT: -16

func main() => integer
begin
    var T: bits(8) = '11110000';

    print(UInt(T));
    print(SInt(T));
    return 0;
end
