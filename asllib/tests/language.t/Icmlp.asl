// RUN: not interp %s | FileCheck %s

func test(N: integer, a: bits(N))
begin
    pass;
end

func main() => integer
begin
    var a: integer = 4;
    test(a, '1111');
    return 0;
end
