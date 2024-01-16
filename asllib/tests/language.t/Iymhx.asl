// RUN: not interp %s | FileCheck %s

func test{M: integer}(a: bits(M + 1)) => bits(M)
begin
    return a[M:0];
end

func main() => integer
begin
    return 0;
end
