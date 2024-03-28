// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => bits(N)
begin
    return a;
end

func main() => integer
begin
    return 0;
end
