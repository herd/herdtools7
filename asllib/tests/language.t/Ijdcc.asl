// RUN: not interp %s | FileCheck %s

type a of bits(5) {
    [10:7] aa
};

func main() => integer
begin
    return 0;
end
