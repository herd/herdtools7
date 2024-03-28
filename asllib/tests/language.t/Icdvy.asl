// RUN: interp %s | FileCheck %s

func test{N:integer{}}(a: bits(N))
begin
    return;
end

func main() => integer
begin
    constant a: integer{0..10} = 1;
    var b: bits(a);
    test(b);
    return 0;
end
