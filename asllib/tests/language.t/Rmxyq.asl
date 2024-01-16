// RUN: interp %s | FileCheck %s

type a of bits(10) {
    [5:0] a,
    [4] b
};

func main() => integer
begin
    var ab : a = '1111 1111 11';
    var abc : bits(6) = ab.a;
    ab.b = '0';

    return 0;
end
