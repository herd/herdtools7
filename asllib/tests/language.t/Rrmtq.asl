// RUN: interp %s | FileCheck %s

type a of bits(6) {
    [3, 1:0] aa,
};

func main() => integer
begin
    return 0;
end
