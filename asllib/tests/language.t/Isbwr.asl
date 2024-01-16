// RUN: interp %s | FileCheck %s

func test(a: bits({0..3}))
begin
    pass;
end

func main() => integer
begin
    var a: bits({1..3}) = '11';
    test(a);
    return 0;
end
