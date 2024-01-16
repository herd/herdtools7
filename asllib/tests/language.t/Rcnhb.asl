// RUN: not interp %s | FileCheck %s

type a of bits(5) {
    [3:0, 3:1] b
};

func main() => integer
begin
    return 0;
end
