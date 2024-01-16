// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => bits(N)
begin
    return a;
end

func main() => integer
begin
    var a = test('1111 0000');

    return 0;
end
