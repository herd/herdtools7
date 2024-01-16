// RUN: interp %s | FileCheck %s

func test(a: integer, b: integer) => bits(a + b)
begin
    return [Zeros(a), Zeros(b)];
end

func main() => integer
begin
    return 0;
end
