// RUN: interp %s | FileCheck %s

func test{N: integer{1..5}}(a: bits(N))
begin
    var b: bits(N+2) = [a, '11'];
end

func main() => integer
begin
    return 0;
end
