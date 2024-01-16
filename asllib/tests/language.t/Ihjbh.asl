// RUN: interp %s | FileCheck %s

// CHECK: 99999999999999999999999999999999999999999999999
// CHECK-NEXT: -99999999999999999999999999999999999999999999999

func main() => integer
begin
    // Does not exactly test for a minimum and a maximum but these break through 32 bit and 64 bit values
    var a: integer = 99999999999999999999999999999999999999999999999;
    var b: integer = -99999999999999999999999999999999999999999999999;

    print(a);
    print(b);

    return 0;
end
