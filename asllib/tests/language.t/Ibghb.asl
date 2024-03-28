// RUN: interp %s | FileCheck %s

type b of bits(10) {
    [0] a
};

func main() => integer
begin
    return 0;
end
