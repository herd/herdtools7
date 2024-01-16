// RUN: not interp %s | FileCheck %s

type a of bits(5) {
    [0] aa,
    [1] aa
};

func main() => integer
begin
    return 0;
end
