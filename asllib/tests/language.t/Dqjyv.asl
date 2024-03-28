// RUN: interp %s | FileCheck %s

config a: integer = 10;

func main() => integer
begin
    assert(TRUE);
    assert(1 == 1);
    assert(a == 10);
    return 0;
end
