// RUN: interp %s | FileCheck %s

type a of bits(4) {
    [0] b
};

func main() => integer
begin
    return 0;
end
