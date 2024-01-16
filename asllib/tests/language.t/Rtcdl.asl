// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => integer{N}
begin
    return N;
end

func main() => integer
begin
    return 0;
end
