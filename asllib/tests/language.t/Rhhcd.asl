// RUN: interp %s | FileCheck %s

func test {N} (a: bits(N))
begin
    var b: array[N] of integer;
end

func main() => integer
begin
    return 0;
end
