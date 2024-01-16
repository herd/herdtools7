// RUN: not interp %s | FileCheck %s

func test(a: bits({2, 4}))
begin
    var b: bits(4) = a;
end

func main() => integer
begin
    return 0;
end
