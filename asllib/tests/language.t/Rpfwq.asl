// RUN: not interp %s | FileCheck %s

func test(N: integer, a: bits(N))
begin
    pass;
end

func main() => integer
begin
    test(10, '1111');
    return 0;
end
