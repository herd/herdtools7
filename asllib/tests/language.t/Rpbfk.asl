// RUN: interp %s | FileCheck %s
// CHECK: 0x0
// CHECK-NEXT: 0xF
// CHECK-NEXT: 0x0

func main() => integer
begin
    print('0000');
    print('1111');
    print('0 0 0 0');
    return 0;
end
