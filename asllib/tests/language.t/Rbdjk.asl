// RUN: not interp %s | FileCheck %s

type a of bits(4) {
    [1:3] b
};

func main() => integer
begin
    return 0;
end
