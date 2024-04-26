// RUN: interp %s | FileCheck %s

func test(a: integer, b: integer{0..3}) => bits(a)
begin
    var c = a + b;
    return Zeros(a);
end

func main() => integer
begin
    return 0;
end