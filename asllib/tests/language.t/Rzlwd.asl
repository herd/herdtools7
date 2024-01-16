// RUN: not interp %s | FileCheck %s

func test{N: integer{5..10}}(a: bits(N))
begin
    pass;
end

func main() => integer
begin
    test('11');
    return 0;
end
