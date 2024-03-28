// RUN: interp %s | FileCheck %s

type a of bits(5) {
    [3:0] a,
    [3:1] b
};

func main() => integer
begin
    return 0;
end
