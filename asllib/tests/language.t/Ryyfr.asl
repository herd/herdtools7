// RUN: not interp %s | FileCheck %s

type a of bits(4) {
    [0] aa,
    [0] bb
};

func main() => integer
begin
    var b: a;
    b.[aa, bb] = '11';

    return 0;
end
