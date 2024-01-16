// RUN: interp %s | FileCheck %s

func test{N: integer{}}(a: bits(N))
begin
    var b = [a, '11'];
end

func main() => integer
begin
    return 0;
end
