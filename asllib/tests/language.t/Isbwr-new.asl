// RUN: interp %s | FileCheck %s

func test{N: integer{0..3}}(a: bits(N))
begin
    pass;
end

func main() => integer
begin
    var a: bits(2) = '11';
    test(a);
    return 0;
end