// RUN: interp %s | FileCheck %s

type a of bits(6) {
    [3:0] aa,
    [4] bb,
    [5] cc
};

func main() => integer
begin
    return 0;
end
