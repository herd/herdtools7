// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (10 ^ 3) == 1000;
    assert (2  ^ 5) == 32;
    return 0;
end