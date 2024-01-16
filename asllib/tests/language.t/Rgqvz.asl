// RUN: interp %s | FileCheck %s
// CHECK: TRUE
// CHECK-NEXT: TRUE

type myBits of bits(6) {
    [0] a,
    [1] b,
    [2] c,
    [3] d,
    [4] e,
    [5] f
};

func main() => integer
begin
    var a : myBits = '101010';
    print(((a.f as bits(1)) == '1'));
    a.f = '0';
    print(((a.f as bits(1)) == '0'));

    return 0;
end
