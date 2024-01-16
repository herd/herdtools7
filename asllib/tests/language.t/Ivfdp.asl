// RUN: not interp %s | FileCheck %s

func test{N:integer{5..10}}(a: bits(N)) => integer{N}
begin
    return N;
end

func main() => integer
begin
    var a = test('1111');
    return 0;
end
