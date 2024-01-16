// RUN: not interp %s | FileCheck %s

func test{N:integer}(a: bits(N), b: bits(N))
begin
    pass;
end

func main() => integer
begin
    test('1', '11');
    return 0;
end
