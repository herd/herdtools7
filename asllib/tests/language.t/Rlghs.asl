// RUN: not interp %s | FileCheck %s

type a of bits(4) {
    -3 b
};

func main() => integer
begin
    return 0;
end
