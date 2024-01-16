// RUN: interp %s | FileCheck %s

type aa of integer;

type a of bits(5) {
    [0] aa
};

func main() => integer
begin
    return 0;
end
