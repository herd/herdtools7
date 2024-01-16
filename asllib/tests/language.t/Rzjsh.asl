// RUN: interp %s | FileCheck %s

type a of bits(6) {
    [4:0] aa,
};

func main() => integer
begin
    var b: a;
    var c: bits(5) = b.aa;
    return 0;
end
